///|

///|
priv enum StartingQuote {
  Single
  Double
} derive(Show, Eq)

///|
priv enum TextLexerState {
  Normal(start~ : UInt)
  EscapeSequence(start~ : UInt)
} derive(Show, Eq)

///|
enum LexerReadingState {
  Start
  Text(StartingQuote, String, TextLexerState)
  True(String)
  False(String)
  Null(String)
  Number(Array[Char], start_offset~ : UInt)
  Decimal(Array[Char], Int, start_offset~ : UInt)
} derive(Show, Eq)

///|
priv struct Lexer {
  state : LexerReadingState
  current_offset : UInt
  target_text : String
  read_tokens : Array[Token]
} derive(Show)

///|
fn Lexer::init(text : String) -> Lexer {
  return {
    state: Start,
    current_offset: 0,
    target_text: text + "\u0000",
    read_tokens: [],
  }
}

///|
fn Lexer::add_tokens(self : Lexer, tokens : Array[Token]) -> Lexer {
  return { ..self, read_tokens: self.read_tokens + tokens }
}

///|
fn Lexer::update_state(self : Lexer, state : LexerReadingState) -> Lexer {
  return { ..self, state, }
}

///|

///|
fn Lexer::set_offset(self : Lexer, offset : UInt) -> Lexer {
  // if self.target_text.length().reinterpret_as_uint() < offset {
  //   fail("hoge~")
  // }
  return { ..self, current_offset: offset }
}

///|
fn Lexer::unexpected_error(self : Lexer) -> LexerErrors {
  return UnexpectedCharacter(
    self.current_char().to_string(),
    self.current_offset,
  )
}

///|
fn Lexer::current_char(self : Lexer) -> Char {
  return self.target_text
    .charcode_at(self.current_offset.reinterpret_as_int())
    .to_char()
    .unwrap() // set_offset時にoffsetの正しさは検証しているので、target_textが不正に書き換えられていない限りunwrapして問題ない
  //
}

///|
pub suberror LexerErrors {
  UnexpectedCharacter(String, UInt)
} derive(Eq, Show)

///|
pub fn lex(
  text : String
) -> (Array[Token], LexerReadingState) raise LexerErrors {
  let mut lexer = Lexer::init(text)
  let lex_true = lex_fixed_word(
    "true",
    fn(i) { Boolean(true, start_offset=i) },
    fn(loaded_chars) { True(loaded_chars) },
  )
  let lex_false = lex_fixed_word(
    "false",
    fn(i) { Boolean(false, start_offset=i) },
    fn(loaded_chars) { False(loaded_chars) },
  )
  let lex_null = lex_fixed_word("null", fn(i) { Null(start_offset=i) }, fn(
    loaded_chars
  ) {
    Null(loaded_chars)
  })
  let mut infinity_blocker = 0
  while lexer.current_offset !=
        lexer.target_text.char_length().reinterpret_as_uint() {
    if infinity_blocker == 1000 {
      println("infinity_blocker reached")
      println("lexer: \{lexer}")
      println("text: \{text}")
      println("text length: \{text.length()}")
      break
    }
    let (updated_i, updated_state, additional_tokens) = match lexer.state {
      Start => lex_start(lexer)
      Text(quote, loaded, text_lexer_state) =>
        lex_text(text_lexer_state, quote, loaded, lexer)
      Number(number, start_offset~) => lex_number(number, lexer, start_offset)
      Decimal(decimal, e, start_offset~) =>
        lex_decimal(lexer, decimal, e, start_offset)
      True(loaded_chars) => lex_true(loaded_chars, lexer)
      False(loaded_chars) => lex_false(loaded_chars, lexer)
      Null(loaded_chars) => lex_null(loaded_chars, lexer)
    }
    lexer = lexer
      .update_state(updated_state)
      .add_tokens(additional_tokens)
      .set_offset(updated_i)
    infinity_blocker += 1
  }
  lexer = lexer.update_state(Start)
  return (lexer.read_tokens, lexer.state)
}

///|
fn lex_start(
  lexer : Lexer
) -> (UInt, LexerReadingState, Array[Token]) raise LexerErrors {
  (
    lexer.current_offset + 1,
    match lexer.current_char() {
      '{' | '}' | '[' | ']' | ':' | ',' => Start
      '"' => Text(Double, "", Normal(start=lexer.current_offset + 1))
      '\'' => Text(Single, "", Normal(start=lexer.current_offset + 1))
      't' => True(['t'])
      'f' => False(['f'])
      'n' => Null(['n'])
      '\u0000' => Start
      _ if lexer.current_char().is_ascii_digit() =>
        Number([lexer.current_char()], start_offset=lexer.current_offset)
      _ if lexer.current_char().is_whitespace() => // Ignore whitespace characters
        Start
      _ => raise lexer.unexpected_error()
    },
    match lexer.current_char() {
      '{' => [BraceStart(start_offset=lexer.current_offset)]
      '}' => [BraceEnd(start_offset=lexer.current_offset)]
      '[' => [BracketStart(start_offset=lexer.current_offset)]
      ']' => [BracketEnd(start_offset=lexer.current_offset)]
      ':' => [Colon(start_offset=lexer.current_offset)]
      ',' => [Comma(start_offset=lexer.current_offset)]
      '"' => [DoubleQuotation(start_offset=lexer.current_offset)]
      '\'' => [SingleQuotation(start_offset=lexer.current_offset)]
      't' | 'f' | 'n' => []
      '\u0000' => []
      _ if lexer.current_char().is_ascii_digit() => []
      _ if lexer.current_char().is_whitespace() => []
      _ => raise lexer.unexpected_error()
    },
  )
}

///|
fn lex_text(
  state : TextLexerState,
  quote : StartingQuote,
  loaded : String,
  lexer : Lexer
) -> (UInt, LexerReadingState, Array[Token]) {
  (
    lexer.current_offset + 1,
    match state {
      Normal(start~) =>
        match lexer.current_char() {
          '\"' => Start
          '\'' => Start
          '\\' => Text(quote, loaded, EscapeSequence(start~))
          _ =>
            Text(
              quote,
              loaded + lexer.current_char().to_string(),
              Normal(start~),
            )
        }
      EscapeSequence(start~) =>
        Text(quote, loaded + lexer.current_char().to_string(), Normal(start~))
    },
    match state {
      Normal(start~) =>
        match lexer.current_char() {
          '\"' =>
            [
              Text(loaded, start_offset=start),
              DoubleQuotation(start_offset=lexer.current_offset),
            ]
          '\'' =>
            [
              Text(loaded, start_offset=start),
              SingleQuotation(start_offset=lexer.current_offset),
            ]
          '\\' | _ => []
        }
      EscapeSequence(_) => []
    },
  )
}

///|
fn lex_number(
  number_list : Array[Char],
  lexer : Lexer,
  start_offset : UInt
) -> (UInt, LexerReadingState, Array[Token]) raise LexerErrors {
  match lexer.current_char() {
    '.' =>
      (
        lexer.current_offset + 1,
        Decimal(number_list, number_list.length(), start_offset~),
        [],
      )
    _ if lexer.current_char().is_ascii_digit() =>
      (
        lexer.current_offset + 1,
        Number([..number_list, lexer.current_char()], start_offset~),
        [],
      )
    _ =>
      (
        lexer.current_offset,
        Start,
        [
          Token::Number(
            Token::to_int(number_list) catch {
              UnexpectedCharacter(t, offset) =>
                raise UnexpectedCharacter(t, offset + lexer.current_offset)
            },
            start_offset~,
          ),
        ],
      )
  }
}

///|
fn lex_decimal(
  lexer : Lexer,
  decimal : Array[Char],
  e : Int,
  start_offset : UInt
) -> (UInt, LexerReadingState, Array[Token]) raise LexerErrors {
  match lexer.current_char() {
    _ if lexer.current_char().is_ascii_digit() =>
      (
        lexer.current_offset + 1,
        Decimal([..decimal, lexer.current_char()], e, start_offset~),
        [],
      )
    _ =>
      (
        lexer.current_offset,
        Start,
        [Decimal(Token::to_double(decimal, e), start_offset~)],
      )
  }
}

///|
fn lex_fixed_word(
  target_word : String,
  target_token_generator : (UInt) -> Token,
  target_state : (String) -> LexerReadingState
) -> (String, Lexer) -> (UInt, LexerReadingState, Array[Token]) raise LexerErrors {
  (loaded_chars : String, lexer : Lexer) => {
    let loaded = loaded_chars + lexer.current_char().to_string()
    match loaded {
      _ if loaded == target_word =>
        (
          lexer.current_offset + 1,
          Start,
          [
            target_token_generator(
              lexer.current_offset +
              1 -
              target_word.length().reinterpret_as_uint(),
            ),
          ],
        )
      _ if target_word.contains(loaded) =>
        (lexer.current_offset + 1, target_state(loaded), [])
      _ => raise UnexpectedCharacter(loaded, lexer.current_offset)
    }
  }
}

///|
test "start" {
  assert_eq(lex("{"), ([Token::BraceStart(start_offset=0)], Start))
  assert_eq(lex("}"), ([Token::BraceEnd(start_offset=0)], Start))
  assert_eq(lex("["), ([Token::BracketStart(start_offset=0)], Start))
  assert_eq(lex("]"), ([Token::BracketEnd(start_offset=0)], Start))
  assert_eq(
    lex("\"\""),
    (
      [
        DoubleQuotation(start_offset=0),
        Text("", start_offset=1),
        DoubleQuotation(start_offset=1),
      ],
      Start,
    ),
  )
  assert_eq(
    lex("''"),
    (
      [
        SingleQuotation(start_offset=0),
        Text("", start_offset=1),
        SingleQuotation(start_offset=1),
      ],
      Start,
    ),
  )
  assert_eq(lex(","), ([Token::Comma(start_offset=0)], Start))
  assert_eq(lex(":"), ([Token::Colon(start_offset=0)], Start))
}

///|
test "string" {
  // double quotation
  assert_eq(
    lex("\"text\""),
    (
      [
        Token::DoubleQuotation(start_offset=0),
        Token::Text("text", start_offset=1),
        Token::DoubleQuotation(start_offset=5),
      ],
      Start,
    ),
  )

  // single quotation
  assert_eq(
    lex("'text'"),
    (
      [
        Token::SingleQuotation(start_offset=0),
        Token::Text("text", start_offset=1),
        Token::SingleQuotation(start_offset=5),
      ],
      Start,
    ),
  )

  // escape sequence
  assert_eq(
    lex("'\\\"'"),
    (
      [
        Token::SingleQuotation(start_offset=0),
        Token::Text("\"", start_offset=1),
        Token::SingleQuotation(start_offset=3),
      ],
      Start,
    ),
  )

  // with white space
  assert_eq(
    lex(
      (
        #|'text with white space'
      ),
    ),
    (
      [
        Token::SingleQuotation(start_offset=0),
        Token::Text("text with white space", start_offset=1),
        Token::SingleQuotation(start_offset=22),
      ],
      Start,
    ),
  )
}

///|
fn assert_unexpected(input : String, expected : (String, UInt)) -> Unit raise {
  match (try? lex(input)) {
    Ok(_) => assert_false(false)
    Err(UnexpectedCharacter(x, y)) => assert_eq((x, y), expected)
  }
}

///|
test "number" {
  assert_eq(lex("1"), ([Number(1, start_offset=0)], Start))
  assert_eq(lex("123"), ([Number(123, start_offset=0)], Start))
}

///|
test "decimal" {
  assert_eq(lex("0.1"), ([Decimal(0.1, start_offset=0)], Start))
  assert_eq(lex("1.1"), ([Decimal(1.1, start_offset=0)], Start))
  assert_eq(lex("1.23"), ([Decimal(1.23, start_offset=0)], Start))
  assert_eq(lex("12.3"), ([Decimal(12.3, start_offset=0)], Start))
}

///|
test "boolean" {
  assert_eq(lex("true"), ([Token::Boolean(true, start_offset=0)], Start))
  assert_eq(lex("false"), ([Token::Boolean(false, start_offset=0)], Start))
  assert_unexpected("ttue", ("tt", 1))
  assert_unexpected("trua", ("trua", 3))
  assert_unexpected("falsa", ("falsa", 4))
}

///|
test "null" {
  assert_eq(lex("null"), ([Token::Null(start_offset=0)], Start))
  assert_unexpected("nall", ("na", 1))
  assert_unexpected("nuli", ("nuli", 3))
  assert_unexpected("nula", ("nula", 3))
  assert_unexpected("nullx", ("x", 4))
}

///|
test "array" {
  assert_eq(
    lex("[11,22,33]"),
    (
      [
        BracketStart(start_offset=0),
        Number(11, start_offset=1),
        Comma(start_offset=3),
        Number(22, start_offset=4),
        Comma(start_offset=6),
        Number(33, start_offset=7),
        BracketEnd(start_offset=9),
      ],
      Start,
    ),
  )
}

///|
test "lex" {
  assert_eq(
    lex(
      "{'decimal_test':12.3,\"number_test\":456,'array_test':['element1',true,false,null]}",
    ),
    (
      [
        BraceStart(start_offset=0),
        SingleQuotation(start_offset=1),
        Text("decimal_test", start_offset=2),
        SingleQuotation(start_offset=14),
        Colon(start_offset=15),
        Decimal(12.3, start_offset=16),
        Comma(start_offset=20),
        DoubleQuotation(start_offset=21),
        Text("number_test", start_offset=22),
        DoubleQuotation(start_offset=33),
        Colon(start_offset=34),
        Number(456, start_offset=35),
        Comma(start_offset=38),
        SingleQuotation(start_offset=39),
        Text("array_test", start_offset=40),
        SingleQuotation(start_offset=50),
        Colon(start_offset=51),
        BracketStart(start_offset=52),
        SingleQuotation(start_offset=53),
        Text("element1", start_offset=54),
        SingleQuotation(start_offset=62),
        Comma(start_offset=63),
        Boolean(true, start_offset=64),
        Comma(start_offset=68),
        Boolean(false, start_offset=69),
        Comma(start_offset=74),
        Null(start_offset=75),
        BracketEnd(start_offset=79),
        BraceEnd(start_offset=80),
      ],
      Start,
    ),
  )
}

///|
test "lex with white space" {
  assert_eq(
    lex(
      (
        #|{
        #|  'decimal_test' : 12.3,
        #|  "number_test":456,
        #|  'array_test':[
        #|    'element1',
        #|    true,
        #|    false,
        #|    null
        #|  ]
        #|}
      ),
    ),
    (
      [
        BraceStart(start_offset=0),
        SingleQuotation(start_offset=4),
        Text("decimal_test", start_offset=5),
        SingleQuotation(start_offset=17),
        Colon(start_offset=19),
        Decimal(12.3, start_offset=21),
        Comma(start_offset=25),
        DoubleQuotation(start_offset=29),
        Text("number_test", start_offset=30),
        DoubleQuotation(start_offset=41),
        Colon(start_offset=42),
        Number(456, start_offset=43),
        Comma(start_offset=46),
        SingleQuotation(start_offset=50),
        Text("array_test", start_offset=51),
        SingleQuotation(start_offset=61),
        Colon(start_offset=62),
        BracketStart(start_offset=63),
        SingleQuotation(start_offset=69),
        Text("element1", start_offset=70),
        SingleQuotation(start_offset=78),
        Comma(start_offset=79),
        Boolean(true, start_offset=85),
        Comma(start_offset=89),
        Boolean(false, start_offset=95),
        Comma(start_offset=100),
        Null(start_offset=106),
        BracketEnd(start_offset=113),
        BraceEnd(start_offset=115),
      ],
      Start,
    ),
  )
}
