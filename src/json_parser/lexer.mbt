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
  Number(Array[Char], start~ : UInt)
  Decimal(Array[Char], Int, start~ : UInt)
} derive(Show, Eq)

///|
pub suberror LexerErrors {
  UnexpectedCharacter(String, Int)
} derive(Eq, Show)

///|
pub fn lex(
  text : String
) -> (Array[Token], LexerReadingState) raise LexerErrors {
  let tokens : Array[Token] = []
  let mut state : LexerReadingState = Start
  let lex_true = lex_fixed_word("true", fn(i) { Boolean(true, pos=i) }, fn(
    loaded_chars
  ) {
    True(loaded_chars)
  })
  let lex_false = lex_fixed_word("false", fn(i) { Boolean(false, pos=i) }, fn(
    loaded_chars
  ) {
    False(loaded_chars)
  })
  let lex_null = lex_fixed_word("null", fn(i) { Null(pos=i) }, fn(
    loaded_chars
  ) {
    Null(loaded_chars)
  })
  let terminated_text = text + "\u0000"
  let mut offset : Int = 0 // UIntを使いたいところだが、文字列へのアクセスにはIntを使うので、Intで計算
  let mut infinity_blocker = 0
  while offset != terminated_text.char_length() {
    if infinity_blocker == 1000 {
      println("infinity_blocker reached")
      println("state: " + state.to_string())
      println("i: " + offset.to_string())
      println("terminated_text: " + terminated_text)
      println("tokens: " + tokens.to_string())
      println("text: " + text)
      println("text length: " + text.length().to_string())
      println("terminated_text length: " + terminated_text.length().to_string())
      break
    }
    let current = terminated_text.char_at(offset)
    let (updated_i, updated_state, additional_tokens) = match state {
      Start => lex_start(current, offset)
      Text(quote, loaded, text_lexer_state) =>
        lex_text(text_lexer_state, quote, loaded, current, offset)
      Number(number, start~) => lex_number(number, current, offset, start)
      Decimal(decimal, e, start~) =>
        match current {
          _ if current.is_ascii_digit() =>
            (offset + 1, Decimal([..decimal, current], e, start~), [])
          _ =>
            (offset, Start, [Decimal(Token::to_double(decimal, e), pos=start)])
        }
      True(loaded_chars) => lex_true(loaded_chars, current, offset)
      False(loaded_chars) => lex_false(loaded_chars, current, offset)
      Null(loaded_chars) => lex_null(loaded_chars, current, offset)
    }
    state = updated_state
    tokens.push_iter(additional_tokens.iter())
    offset = updated_i
    infinity_blocker += 1
  }
  state = Start
  return (tokens, state)
}

///|
fn lex_start(
  current : Char,
  offset : Int
) -> (Int, LexerReadingState, Array[Token]) raise LexerErrors {
  (
    offset + 1,
    match current {
      '{' | '}' | '[' | ']' | ':' | ',' => Start
      '"' => Text(Double, "", Normal(start=offset.reinterpret_as_uint() + 1))
      '\'' => Text(Single, "", Normal(start=offset.reinterpret_as_uint() + 1))
      't' => True(['t'])
      'f' => False(['f'])
      'n' => Null(['n'])
      '\u0000' => Start
      _ if current.is_ascii_digit() =>
        Number([current], start=offset.reinterpret_as_uint())
      _ if current.is_whitespace() => // Ignore whitespace characters
        Start
      _ => raise UnexpectedCharacter(current.to_string(), offset)
    },
    match current {
      '{' => [BraceStart(pos=offset.reinterpret_as_uint())]
      '}' => [BraceEnd(pos=offset.reinterpret_as_uint())]
      '[' => [BracketStart(pos=offset.reinterpret_as_uint())]
      ']' => [BracketEnd(pos=offset.reinterpret_as_uint())]
      ':' => [Colon(pos=offset.reinterpret_as_uint())]
      ',' => [Comma(pos=offset.reinterpret_as_uint())]
      '"' => [DoubleQuotation(pos=offset.reinterpret_as_uint())]
      '\'' => [SingleQuotation(pos=offset.reinterpret_as_uint())]
      't' | 'f' | 'n' => []
      '\u0000' => []
      _ if current.is_ascii_digit() => []
      _ if current.is_whitespace() => []
      _ => raise UnexpectedCharacter(current.to_string(), offset)
    },
  )
}

///|
fn lex_text(
  state : TextLexerState,
  quote : StartingQuote,
  loaded : String,
  current : Char,
  offset : Int
) -> (Int, LexerReadingState, Array[Token]) {
  (
    offset + 1,
    match state {
      Normal(start~) =>
        match current {
          '\"' => LexerReadingState::Start
          '\'' => LexerReadingState::Start
          '\\' => LexerReadingState::Text(quote, loaded, EscapeSequence(start~))
          _ =>
            LexerReadingState::Text(
              quote,
              loaded + current.to_string(),
              Normal(start~),
            )
        }
      EscapeSequence(start~) =>
        LexerReadingState::Text(
          quote,
          loaded + current.to_string(),
          Normal(start~),
        )
    },
    match state {
      Normal(start=pos) =>
        match current {
          '\"' =>
            [
              Text(loaded, pos~),
              DoubleQuotation(pos=offset.reinterpret_as_uint()),
            ]
          '\'' =>
            [
              Text(loaded, pos~),
              SingleQuotation(pos=offset.reinterpret_as_uint()),
            ]
          '\\' | _ => []
        }
      EscapeSequence => []
    },
  )
}

///|
fn lex_number(
  number_list : Array[Char],
  current : Char,
  current_offset : Int,
  start_offset : UInt
) -> (Int, LexerReadingState, Array[Token]) raise LexerErrors {
  match current {
    '.' =>
      (
        current_offset + 1,
        Decimal(number_list, number_list.length(), start=start_offset),
        [],
      )
    _ if current.is_ascii_digit() =>
      (
        current_offset + 1,
        Number([..number_list, current], start=start_offset),
        [],
      )
    _ =>
      (
        current_offset,
        Start,
        [
          Token::Number(
            Token::to_int(number_list) catch {
              UnexpectedCharacter(t, _i) =>
                raise UnexpectedCharacter(t, _i + current_offset)
            },
            pos=start_offset,
          ),
        ],
      )
  }
}

///|
fn lex_fixed_word(
  target_word : String,
  target_token_generator : (UInt) -> Token,
  target_state : (String) -> LexerReadingState
) -> (String, Char, Int) -> (Int, LexerReadingState, Array[Token]) raise LexerErrors {
  (loaded_chars : String, c : Char, i : Int) => {
    let loaded = loaded_chars + c.to_string()
    match loaded {
      _ if loaded == target_word =>
        (
          i + 1,
          Start,
          [
            target_token_generator(
              (i + 1 - target_word.length()).reinterpret_as_uint(),
            ),
          ],
        )
      _ if target_word.contains(loaded) => (i + 1, target_state(loaded), [])
      _ => raise UnexpectedCharacter(loaded, i)
    }
  }
}

///|
test "start" {
  assert_eq(lex("{"), ([Token::BraceStart(pos=0)], Start))
  assert_eq(lex("}"), ([Token::BraceEnd(pos=0)], Start))
  assert_eq(lex("["), ([Token::BracketStart(pos=0)], Start))
  assert_eq(lex("]"), ([Token::BracketEnd(pos=0)], Start))
  assert_eq(
    lex("\"\""),
    ([DoubleQuotation(pos=0), Text("", pos=1), DoubleQuotation(pos=1)], Start),
  )
  assert_eq(
    lex("''"),
    ([SingleQuotation(pos=0), Text("", pos=1), SingleQuotation(pos=1)], Start),
  )
  assert_eq(lex(","), ([Token::Comma(pos=0)], Start))
  assert_eq(lex(":"), ([Token::Colon(pos=0)], Start))
}

///|
test "string" {
  // double quotation
  assert_eq(
    lex("\"text\""),
    (
      [
        Token::DoubleQuotation(pos=0),
        Token::Text("text", pos=1),
        Token::DoubleQuotation(pos=5),
      ],
      Start,
    ),
  )

  // single quotation
  assert_eq(
    lex("'text'"),
    (
      [
        Token::SingleQuotation(pos=0),
        Token::Text("text", pos=1),
        Token::SingleQuotation(pos=5),
      ],
      Start,
    ),
  )

  // escape sequence
  assert_eq(
    lex("'\\\"'"),
    (
      [
        Token::SingleQuotation(pos=0),
        Token::Text("\"", pos=1),
        Token::SingleQuotation(pos=3),
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
        Token::SingleQuotation(pos=0),
        Token::Text("text with white space", pos=1),
        Token::SingleQuotation(pos=22),
      ],
      Start,
    ),
  )
}

///|
fn assert_unexpected(input : String, expected : (String, Int)) -> Unit raise {
  match (try? lex(input)) {
    Ok(_) => assert_false(false)
    Err(UnexpectedCharacter(x, y)) => assert_eq((x, y), expected)
  }
}

///|
test "number" {
  assert_eq(lex("1"), ([Number(1, pos=0)], Start))
  assert_eq(lex("123"), ([Number(123, pos=0)], Start))
}

///|
test "decimal" {
  assert_eq(lex("0.1"), ([Decimal(0.1, pos=0)], Start))
  assert_eq(lex("1.1"), ([Decimal(1.1, pos=0)], Start))
  assert_eq(lex("1.23"), ([Decimal(1.23, pos=0)], Start))
  assert_eq(lex("12.3"), ([Decimal(12.3, pos=0)], Start))
}

///|
test "boolean" {
  assert_eq(lex("true"), ([Token::Boolean(true, pos=0)], Start))
  assert_eq(lex("false"), ([Token::Boolean(false, pos=0)], Start))
  assert_unexpected("ttue", ("tt", 1))
  assert_unexpected("trua", ("trua", 3))
  assert_unexpected("falsa", ("falsa", 4))
}

///|
test "null" {
  assert_eq(lex("null"), ([Token::Null(pos=0)], Start))
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
        BracketStart(pos=0),
        Number(11, pos=1),
        Comma(pos=3),
        Number(22, pos=4),
        Comma(pos=6),
        Number(33, pos=7),
        BracketEnd(pos=9),
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
        BraceStart(pos=0),
        SingleQuotation(pos=1),
        Text("decimal_test", pos=2),
        SingleQuotation(pos=14),
        Colon(pos=15),
        Decimal(12.3, pos=16),
        Comma(pos=20),
        DoubleQuotation(pos=21),
        Text("number_test", pos=22),
        DoubleQuotation(pos=33),
        Colon(pos=34),
        Number(456, pos=35),
        Comma(pos=38),
        SingleQuotation(pos=39),
        Text("array_test", pos=40),
        SingleQuotation(pos=50),
        Colon(pos=51),
        BracketStart(pos=52),
        SingleQuotation(pos=53),
        Text("element1", pos=54),
        SingleQuotation(pos=62),
        Comma(pos=63),
        Boolean(true, pos=64),
        Comma(pos=68),
        Boolean(false, pos=69),
        Comma(pos=74),
        Null(pos=75),
        BracketEnd(pos=79),
        BraceEnd(pos=80),
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
        BraceStart(pos=0),
        SingleQuotation(pos=4),
        Text("decimal_test", pos=5),
        SingleQuotation(pos=17),
        Colon(pos=19),
        Decimal(12.3, pos=21),
        Comma(pos=25),
        DoubleQuotation(pos=29),
        Text("number_test", pos=30),
        DoubleQuotation(pos=41),
        Colon(pos=42),
        Number(456, pos=43),
        Comma(pos=46),
        SingleQuotation(pos=50),
        Text("array_test", pos=51),
        SingleQuotation(pos=61),
        Colon(pos=62),
        BracketStart(pos=63),
        SingleQuotation(pos=69),
        Text("element1", pos=70),
        SingleQuotation(pos=78),
        Comma(pos=79),
        Boolean(true, pos=85),
        Comma(pos=89),
        Boolean(false, pos=95),
        Comma(pos=100),
        Null(pos=106),
        BracketEnd(pos=113),
        BraceEnd(pos=115),
      ],
      Start,
    ),
  )
}
