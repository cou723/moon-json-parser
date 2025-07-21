///|
pub enum Token {
  Text(String, pos~ : UInt)
  Number(Int, pos~ : UInt)
  Decimal(Double, pos~ : UInt)
  Boolean(Bool, pos~ : UInt)
  Null(pos~ : UInt)
  BraceStart(pos~ : UInt) // {
  BraceEnd(pos~ : UInt) // }
  BracketStart(pos~ : UInt) // [
  BracketEnd(pos~ : UInt) // ]
  DoubleQuotation(pos~ : UInt)
  SingleQuotation(pos~ : UInt)
  Comma(pos~ : UInt)
  Colon(pos~ : UInt)
} derive(Show, Eq)

///|
pub enum TokenKind {
  Text
  Number
  Decimal
  Boolean
  Null
  BraceStart
  BraceEnd
  BracketStart
  BracketEnd
  DoubleQuotation
  SingleQuotation
  Comma
  Colon
} derive(Show, Eq)

///|
fn Token::to_int(numbers : Array[Char]) -> Int raise LexerErrors {
  // foldiにraisableなコールバックを渡す方法がわからない
  numbers.foldi(init=0, fn(i, sum, c) {
    sum +
    10.0.pow((numbers.length() - i - 1).to_double()).to_int() *
    (match c {
      n if n.is_numeric() => n.to_int() - 48
      _ => raise UnexpectedCharacter(c.to_string(), i)
    })
  })
}

///|
fn Token::to_double(numbers : Array[Char], i : Int) -> Double raise LexerErrors {
  Token::to_int(numbers).to_double() /
  (10 : Double).pow((numbers.length() - i).to_double())
}

///|
fn Token::to_kind(self : Token) -> TokenKind {
  match self {
    Text(_) => Text
    Number(_) => Number
    Decimal(_) => Decimal
    Boolean(_) => Boolean
    Null(_) => Null
    BraceStart(_) => BraceStart
    BraceEnd(_) => BraceEnd
    BracketStart(_) => BracketStart
    BracketEnd(_) => BracketEnd
    DoubleQuotation(_) => DoubleQuotation
    SingleQuotation(_) => SingleQuotation
    Comma(_) => Comma
    Colon(_) => Colon
  }
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
