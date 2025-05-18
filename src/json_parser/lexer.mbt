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
  Comma // ,
  Colon // :
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
  assert_eq!(Token::to_int(['0']), 0)
  // assert_eq!(Token::to_int(['1']), 1)
  assert_eq!(Token::to_int(['2']), 2)
  assert_eq!(Token::to_int(['1', '0']), 10)
  assert_eq!(Token::to_int(['1', '0', '0']), 100)
  assert_eq!(Token::to_int(['1', '2', '3']), 123)
  assert_eq!(Token::to_int(['3', '2', '1']), 321)
}

///|
enum StartingQuote {
  Single
  Double
} derive(Show, Eq)

///|
enum TextLexerState {
  Normal
  EscapeSequence
} derive(Show, Eq)

///|
enum LexerState {
  Start
  Text(StartingQuote, String, TextLexerState)
  True(String)
  False(String)
  Null(String)
  Number(Array[Char])
  Decimal(Array[Char], Int)
} derive(Show, Eq)

///|
type! LexerErrors {
  UnexpectedCharacter(String)
  ExpectQuote
} derive(Eq, Show)

///|
pub fn lex(text : String) -> (Array[Token], LexerState)!LexerErrors {
  let tokens : Array[Token] = []
  let mut state : LexerState = Start
  for c in text.to_array() {
    match state {
      Start => {
        let (s, t) = lex_start!(c)
        state = s
        if t is Some(token) {
          tokens.push(token)
        }
      }
      Text(quote, loaded, text_lexer_state) => {
        let (s, ts) = lex_text(text_lexer_state, quote, loaded, c)
        state = s
        for t in ts {
          tokens.push(t)
        }
      }
      Number(n) => {
        let (s, ts) = lex_number(n, c)
        state = s
        for t in ts {
          tokens.push(t)
        }
      }
      Decimal(n, i) =>
        state = match c {
          _ if c.is_ascii_digit() => Decimal([..n, c], i)
          _ => {
            tokens.push(Token::Decimal(Token::to_double([..n, c], i)))
            Start
          }
        }
      True(loaded_list) =>
        match loaded_list {
          _ if loaded_list.length() < 4 => state = True([..loaded_list, c])
          _ if loaded_list == "true" => {
            tokens.push(Token::Boolean(true))
            state = Start
          }
          _ =>
            raise UnexpectedCharacter(
              [..loaded_list, c].map(Char::to_string).join(""),
            )
        }
      False(loaded_list) =>
        match loaded_list {
          _ if loaded_list.length() < 5 => state = False([..loaded_list, c])
          _ if loaded_list == "false" => {
            tokens.push(Token::Boolean(false))
            state = Start
          }
          _ =>
            raise UnexpectedCharacter(
              [..loaded_list, c].map(Char::to_string).join(""),
            )
        }
      Null(loaded_list) =>
        match loaded_list {
          _ if loaded_list.length() < 4 => state = Null([..loaded_list, c])
          _ if loaded_list == "null" => {
            tokens.push(Token::Null)
            state = Start
          }
          _ =>
            raise UnexpectedCharacter(
              [..loaded_list, c].map(Char::to_string).join(""),
            )
        }
    }
  }
  // 終了処理
  match state {
    Number(number_list) =>
      tokens.push(Token::Number(Token::to_int(number_list)))
    Decimal(number_list, i) =>
      tokens.push(Token::Decimal(Token::to_double(number_list, i)))
    True(text) =>
      if text == "true" {
        tokens.push(Token::Boolean(true))
      } else {
        raise UnexpectedCharacter(text)
      }
    False(text) =>
      if text == "false" {
        tokens.push(Token::Boolean(false))
      } else {
        raise UnexpectedCharacter(text)
      }
    Null(text) =>
      if text == "null" {
        tokens.push(Token::Null)
      } else {
        raise UnexpectedCharacter(text)
      }
    Text(_, _, _) => raise ExpectQuote
    _ => return (tokens, state)
  }
  state = Start
  return (tokens, state)
}

///|
fn lex_start(c : Char) -> (LexerState, Token?)!LexerErrors {
  (
    match c {
      '{' | '}' | '[' | ']' | ':' | ',' => Start
      '"' => Text(Double, "", Normal)
      '\'' => Text(Single, "", Normal)
      't' => True(['t'])
      'f' => False(['f'])
      'n' => Null(['n'])
      _ if c.is_ascii_digit() => Number([c])
      _ => raise UnexpectedCharacter(c.to_string())
    },
    match c {
      '{' => Some(BraceStart)
      '}' => Some(BraceEnd)
      '[' => Some(BracketStart)
      ']' => Some(BracketEnd)
      ':' => Some(Colon)
      ',' => Some(Comma)
      '"' => Some(DoubleQuotation)
      '\'' => Some(SingleQuotation)
      't' | 'f' | 'n' => None
      _ if c.is_ascii_digit() => None
      _ => raise UnexpectedCharacter(c.to_string())
    },
  )
}

///|
fn lex_text(
  state : TextLexerState,
  quote : StartingQuote,
  loaded : String,
  c : Char
) -> (LexerState, Array[Token]) {
  (
    match state {
      Normal =>
        match c {
          '\"' => LexerState::Start
          '\'' => LexerState::Start
          '\\' => LexerState::Text(quote, loaded, EscapeSequence)
          _ => LexerState::Text(quote, loaded + c.to_string(), Normal)
        }
      EscapeSequence => LexerState::Text(quote, loaded + c.to_string(), Normal)
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
  c : Char
) -> (LexerState, Array[Token]) {
  (
    match c {
      '.' => Decimal([..number_list], number_list.length())
      _ if c.is_ascii_digit() => Number([..number_list, c])
      _ => Start
    },
    match c {
      '.' | _ if c.is_ascii_digit() => []
      _ => [Token::Number(Token::to_int([..number_list, c]))]
    },
  )
}

///|
test "start" {
  assert_eq!(lex!("{"), ([Token::BraceStart], Start))
  assert_eq!(lex!("}"), ([Token::BraceEnd], Start))
  assert_eq!(lex!("["), ([Token::BracketStart], Start))
  assert_eq!(lex!("]"), ([Token::BracketEnd], Start))
  assert_eq!(
    lex!("\"\""),
    ([DoubleQuotation, Text(""), DoubleQuotation], Start),
  )
  assert_eq!(lex!("''"), ([SingleQuotation, Text(""), SingleQuotation], Start))
  assert_eq!(lex!(","), ([Token::Comma], Start))
  assert_eq!(lex!(":"), ([Token::Colon], Start))
}

///|
test "string" {
  // double quotation
  assert_eq!(
    lex!("\"text\""),
    (
      [Token::DoubleQuotation, Token::Text("text"), Token::DoubleQuotation],
      Start,
    ),
  )

  // single quotation
  assert_eq!(
    lex!("'text'"),
    (
      [Token::SingleQuotation, Token::Text("text"), Token::SingleQuotation],
      Start,
    ),
  )

  // escape sequence
  assert_eq!(
    lex!("'\\\"'"),
    ([Token::SingleQuotation, Token::Text("\""), Token::SingleQuotation], Start),
  )
}

///|
test "number" {
  assert_eq!(lex!("1"), ([Number(1)], Start))
  assert_eq!(lex!("123"), ([Number(123)], Start))
}

///|
test "decimal" {
  assert_eq!(lex!("0.1"), ([Decimal(0.1)], Start))
  assert_eq!(lex!("1.1"), ([Decimal(1.1)], Start))
  assert_eq!(lex!("1.23"), ([Decimal(1.23)], Start))
  assert_eq!(lex!("12.3"), ([Decimal(12.3)], Start))
}

///|
test "boolean" {
  assert_eq!(lex!("true"), ([Token::Boolean(true)], Start))
  assert_eq!(lex!("false"), ([Token::Boolean(false)], Start))
  assert_eq!(lex?("ttue"), Err(UnexpectedCharacter("ttue")))
  assert_eq!(lex?("trua"), Err(UnexpectedCharacter("trua")))
  assert_eq!(lex?("falsa"), Err(UnexpectedCharacter("falsa")))
}

///|
test "null" {
  assert_eq!(lex!("null"), ([Token::Null], Start))
  assert_eq!(lex?("mull"), Err(UnexpectedCharacter("m")))
  assert_eq!(lex?("nall"), Err(UnexpectedCharacter("nall")))
  assert_eq!(lex?("nuli"), Err(UnexpectedCharacter("nuli")))
  assert_eq!(lex?("nuli"), Err(UnexpectedCharacter("nuli")))
  assert_eq!(lex?("nullx"), Err(UnexpectedCharacter("x")))
}
