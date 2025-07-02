///|
enum Node {
  Object(Array[(String, Node)])
  String(String)
  Number(Int)
  Decimal(Double)
  Array(Array[Node])
  Null
  Boolean(Bool)
} derive(Eq, Show)

///|
enum ParserState {
  Start
  InArray
  InObject(InObjectState)
} derive(Eq, Show)

///|
enum InObjectState {
  WannaKey
  WannaCoron
  WannaValue
  WannaComma
} derive(Eq, Show)

///|
suberror ParserError {
  UnexpectedToken(Token, Array[Token]) // Actual, Expects
} derive(Eq, Show)

///|

///|
// pub fn parse_json(text : String) -> Json! {
//   return json_from_nodes!(parse!(lex!(text)))
// }

// ///|
// fn json_from_nodes(nodes : Array[Node]) -> Json! {
//   return []
// }

///|
fn parse(tokens : Array[Token]) -> Array[Node] raise ParserError {
  let mut nodes : Array[Node] = []
  let mut state = ParserState::Start
  return [match tokens[0] {
    SingleQuotation | DoubleQuotation => parse_string(tokens)
    BracketStart =>...
    BraceStart => ...
    Null => Node::Null;
    Boolean(b) => Boolean(b)
    Decimal(d) => Decimal(d)
    Number(n) => Number(n)
    _ =>
      raise UnexpectedToken(tokens[0], [
        SingleQuotation,
        DoubleQuotation,
        BracketStart,
        BraceStart,
        Null,
      ])
  }]
}

///|
fn parse_string(tokens : Array[Token]) -> Node raise ParserError {
  match tokens[0] {
    Token::DoubleQuotation =>
      match tokens[0 + 1] {
        Token::Text(text) =>
          match tokens[0 + 2] {
            DoubleQuotation => Node::String(text)
            _ => raise UnexpectedToken(tokens[0 + 2], [DoubleQuotation])
          }
        _ => raise UnexpectedToken(tokens[0 + 2], [DoubleQuotation])
      }
    Token::SingleQuotation =>
      match tokens[0 + 1] {
        Token::Text(text) =>
          match tokens[0 + 2] {
            SingleQuotation => Node::String(text)
            _ => raise UnexpectedToken(tokens[0 + 2], [SingleQuotation])
          }
        _ => raise UnexpectedToken(tokens[0 + 2], [SingleQuotation])
      }
    _ =>
      raise UnexpectedToken(tokens[0 + 2], [SingleQuotation, DoubleQuotation])
  }
}

///|
test "parse_string" {
  assert_eq(
    parse_string(
      [Token::DoubleQuotation, Token::Text("hoge"), Token::DoubleQuotation]
    ),
    Node::String("hoge"),
  )
}

///|
