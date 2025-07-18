///|
enum Token {
  Text(String)
  Number(Int)
  Decimal(Double)
  Boolean(Bool)
  Null
  BraceStart // {
  BraceEnd // }
  BracketStart // [
  BracketEnd // ]
  DoubleQuotation
  SingleQuotation
  Comma
  Colon
} derive(Show, Eq)

///|
fn Token::to_int(numbers : Array[Char]) -> Int {
  // foldiにraisableなコールバックを渡す方法がわからない
  // numbers.foldi?(init=0, fn(i, sum, c) -> Int!UnexpectedCharacter {
  //   sum +
  //   ((numbers.length() - i) ^ 10) *
  //   (match c {
  //     '0' => 0
  //     '1' => 1
  //     _ => raise UnexpectedCharacter(c)
  //   })
  // })
  let mut total = 0
  for i, c in numbers {
    total += (10.0.pow((numbers.length() - i - 1).to_double()) *
    (c.to_int() - 48).to_double()).to_int()
  }
  return total
}

///|
fn Token::to_double(numbers : Array[Char], i : Int) -> Double {
  Token::to_int(numbers).to_double() /
  (10 : Double).pow((numbers.length() - i).to_double())
}

///|
test "to_int" {
  assert_eq(Token::to_int(['0']), 0)
  // assert_eq!(Token::to_int(['1']), 1)
  assert_eq(Token::to_int(['2']), 2)
  assert_eq(Token::to_int(['1', '0']), 10)
  assert_eq(Token::to_int(['1', '0', '0']), 100)
  assert_eq(Token::to_int(['1', '2', '3']), 123)
  assert_eq(Token::to_int(['3', '2', '1']), 321)
}

///|
test "to_double" {
  assert_eq(Token::to_double(['0', '1'], 1), 0.1)
  assert_eq(Token::to_double(['1', '1'], 1), 1.1)
  assert_eq(Token::to_double(['1', '1', '1'], 1), 1.11)
  assert_eq(Token::to_double(['1', '1', '1'], 2), 11.1)
}

///|
priv enum StartingQuote {
  Single
  Double
} derive(Show, Eq)

///|
priv enum TextLexerState {
  Normal
  EscapeSequence
} derive(Show, Eq)

///|
enum LexerReadingState {
  Start
  Text(StartingQuote, String, TextLexerState)
  True(String)
  False(String)
  Null(String)
  Number(Array[Char])
  Decimal(Array[Char], Int)
} derive(Show, Eq)

///|
suberror LexerErrors {
  UnexpectedCharacter(String, Int)
} derive(Eq, Show)

///|
pub fn lex(
  text : String
) -> (Array[Token], LexerReadingState) raise LexerErrors {
  let tokens : Array[Token] = []
  let mut state : LexerReadingState = Start
  let lex_true = lex_fixed_word("true", Boolean(true), fn(loaded_chars) {
    True(loaded_chars)
  })
  let lex_false = lex_fixed_word("false", Boolean(false), fn(loaded_chars) {
    False(loaded_chars)
  })
  let lex_null = lex_fixed_word("null", Null, fn(loaded_chars) {
    Null(loaded_chars)
  })
  let terminated_text = text + "\u0000"
  let mut i : Int = 0 // UIntを使いたいところだが、文字列へのアクセスにはIntを使うので、Intで計算
  let mut infinity_blocker = 0
  while i != terminated_text.char_length() {
    if infinity_blocker == 1000 {
      println("infinity_blocker reached")
      println("state: " + state.to_string())
      println("i: " + i.to_string())
      println("terminated_text: " + terminated_text)
      println("tokens: " + tokens.to_string())
      println("text: " + text)
      println("text length: " + text.length().to_string())
      println("terminated_text length: " + terminated_text.length().to_string())
      break
    }
    let c = terminated_text.char_at(i)
    let (updated_i, updated_state, additional_tokens) = match state {
      Start => lex_start(c, i)
      Text(quote, loaded, text_lexer_state) =>
        lex_text(text_lexer_state, quote, loaded, c, i)
      Number(number) => lex_number(number, c, i)
      Decimal(decimal, e) =>
        match c {
          _ if c.is_ascii_digit() => (i + 1, Decimal([..decimal, c], e), [])
          _ => (i, Start, [Token::Decimal(Token::to_double(decimal, e))])
        }
      True(loaded_chars) => lex_true(loaded_chars, c, i)
      False(loaded_chars) => lex_false(loaded_chars, c, i)
      Null(loaded_chars) => lex_null(loaded_chars, c, i)
    }
    state = updated_state
    tokens.push_iter(additional_tokens.iter())
    i = updated_i
    infinity_blocker += 1
  }
  state = Start
  return (tokens, state)
}

///|
fn lex_start(
  c : Char,
  i : Int
) -> (Int, LexerReadingState, Array[Token]) raise LexerErrors {
  (
    i + 1,
    match c {
      '{' | '}' | '[' | ']' | ':' | ',' => Start
      '"' => Text(Double, "", Normal)
      '\'' => Text(Single, "", Normal)
      't' => True(['t'])
      'f' => False(['f'])
      'n' => Null(['n'])
      '\u0000' => Start
      _ if c.is_ascii_digit() => Number([c])
      _ if c.is_whitespace() => // Ignore whitespace characters
        Start
      _ => raise UnexpectedCharacter(c.to_string(), i)
    },
    match c {
      '{' => [BraceStart]
      '}' => [BraceEnd]
      '[' => [BracketStart]
      ']' => [BracketEnd]
      ':' => [Colon]
      ',' => [Comma]
      '"' => [DoubleQuotation]
      '\'' => [SingleQuotation]
      't' | 'f' | 'n' => []
      '\u0000' => []
      _ if c.is_ascii_digit() => []
      _ if c.is_whitespace() => []
      _ => raise UnexpectedCharacter(c.to_string(), i)
    },
  )
}

///|
fn lex_text(
  state : TextLexerState,
  quote : StartingQuote,
  loaded : String,
  c : Char,
  i : Int
) -> (Int, LexerReadingState, Array[Token]) {
  (
    i + 1,
    match state {
      Normal =>
        match c {
          '\"' => LexerReadingState::Start
          '\'' => LexerReadingState::Start
          '\\' => LexerReadingState::Text(quote, loaded, EscapeSequence)
          _ => LexerReadingState::Text(quote, loaded + c.to_string(), Normal)
        }
      EscapeSequence =>
        LexerReadingState::Text(quote, loaded + c.to_string(), Normal)
    },
    match state {
      Normal =>
        match c {
          '\"' => [Token::Text(loaded), DoubleQuotation]
          '\'' => [Token::Text(loaded), SingleQuotation]
          '\\' | _ => []
        }
      EscapeSequence => []
    },
  )
}

///|
fn lex_number(
  number_list : Array[Char],
  c : Char,
  i : Int
) -> (Int, LexerReadingState, Array[Token]) {
  match c {
    '.' => (i + 1, Decimal(number_list, number_list.length()), [])
    _ if c.is_ascii_digit() => (i + 1, Number([..number_list, c]), [])
    _ => (i, Start, [Token::Number(Token::to_int(number_list))])
  }
}

///|
fn lex_fixed_word(
  target_word : String,
  target_token : Token,
  target_state : (String) -> LexerReadingState
) -> (String, Char, Int) -> (Int, LexerReadingState, Array[Token]) raise LexerErrors {
  (loaded_chars : String, c : Char, i : Int) => {
    let loaded = loaded_chars + c.to_string()
    match loaded {
      _ if loaded == target_word => (i + 1, Start, [target_token])
      _ if target_word.contains(loaded) => (i + 1, target_state(loaded), [])
      _ => raise UnexpectedCharacter(loaded, i)
    }
  }
}

///|
test "start" {
  assert_eq(lex("{"), ([Token::BraceStart], Start))
  assert_eq(lex("}"), ([Token::BraceEnd], Start))
  assert_eq(lex("["), ([Token::BracketStart], Start))
  assert_eq(lex("]"), ([Token::BracketEnd], Start))
  assert_eq(lex("\"\""), ([DoubleQuotation, Text(""), DoubleQuotation], Start))
  assert_eq(lex("''"), ([SingleQuotation, Text(""), SingleQuotation], Start))
  assert_eq(lex(","), ([Token::Comma], Start))
  assert_eq(lex(":"), ([Token::Colon], Start))
}

///|
test "string" {
  // double quotation
  assert_eq(
    lex("\"text\""),
    (
      [Token::DoubleQuotation, Token::Text("text"), Token::DoubleQuotation],
      Start,
    ),
  )

  // single quotation
  assert_eq(
    lex("'text'"),
    (
      [Token::SingleQuotation, Token::Text("text"), Token::SingleQuotation],
      Start,
    ),
  )

  // escape sequence
  assert_eq(
    lex("'\\\"'"),
    ([Token::SingleQuotation, Token::Text("\""), Token::SingleQuotation], Start),
  )

  // with white space
  assert_eq(
    lex(
      #|'text with white space'
      ,
    ),
    (
      [
        Token::SingleQuotation,
        Token::Text("text with white space"),
        Token::SingleQuotation,
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
  assert_eq(lex("1"), ([Number(1)], Start))
  assert_eq(lex("123"), ([Number(123)], Start))
}

///|
test "decimal" {
  assert_eq(lex("0.1"), ([Decimal(0.1)], Start))
  assert_eq(lex("1.1"), ([Decimal(1.1)], Start))
  assert_eq(lex("1.23"), ([Decimal(1.23)], Start))
  assert_eq(lex("12.3"), ([Decimal(12.3)], Start))
}

///|
test "boolean" {
  assert_eq(lex("true"), ([Token::Boolean(true)], Start))
  assert_eq(lex("false"), ([Token::Boolean(false)], Start))
  assert_unexpected("ttue", ("tt", 1))
  assert_unexpected("trua", ("trua", 3))
  assert_unexpected("falsa", ("falsa", 4))
}

///|
test "null" {
  assert_eq(lex("null"), ([Token::Null], Start))
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
        BracketStart,
        Number(11),
        Comma,
        Number(22),
        Comma,
        Number(33),
        BracketEnd,
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
        BraceStart,
        SingleQuotation,
        Text("decimal_test"),
        SingleQuotation,
        Colon,
        Decimal(12.3),
        Comma,
        DoubleQuotation,
        Text("number_test"),
        DoubleQuotation,
        Colon,
        Number(456),
        Comma,
        SingleQuotation,
        Text("array_test"),
        SingleQuotation,
        Colon,
        BracketStart,
        SingleQuotation,
        Text("element1"),
        SingleQuotation,
        Comma,
        Boolean(true),
        Comma,
        Boolean(false),
        Comma,
        Null,
        BracketEnd,
        BraceEnd,
      ],
      Start,
    ),
  )
}

///|
test "lex with white space" {
  assert_eq(
    lex(
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
      ,
    ),
    (
      [
        BraceStart,
        SingleQuotation,
        Text("decimal_test"),
        SingleQuotation,
        Colon,
        Decimal(12.3),
        Comma,
        DoubleQuotation,
        Text("number_test"),
        DoubleQuotation,
        Colon,
        Number(456),
        Comma,
        SingleQuotation,
        Text("array_test"),
        SingleQuotation,
        Colon,
        BracketStart,
        SingleQuotation,
        Text("element1"),
        SingleQuotation,
        Comma,
        Boolean(true),
        Comma,
        Boolean(false),
        Comma,
        Null,
        BracketEnd,
        BraceEnd,
      ],
      Start,
    ),
  )
}
