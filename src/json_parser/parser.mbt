///|
enum Node {
  Object(Map[String, Node])
  String(String)
  Number(Int)
  Decimal(Double)
  Array(Array[Node])
}derive(Eq, Show)

///|
enum ParserState {
  Start
  InArray
  InObject(InObjectState)
}derive(Eq, Show)

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
  let mut i = 0
  let mut nodes : Array[Node] = []
  let mut state = ParserState::Start
  match tokens[0]{
    SingleQuotation |DoubleQuotation=> ...
    BracketStart => ...
    BraceStart => ...
    Null => ...
    Boolean(_) => ...
    Decimal(_) => ...
    Number(_) => ...
    _ => raise UnexpectedToken(tokens[0], [SingleQuotation, DoubleQuotation,BracketStart,BraceStart,Null])
  }
  return []
}

///|
fn parse_string(
  tokens : Array[Token],
  i : Int
) -> (Node, Int) raise ParserError {
  match tokens[i] {
    Token::DoubleQuotation =>
      match tokens[i + 1] {
        Token::Text(text) =>
          match tokens[i + 2] {
            DoubleQuotation => (Node::String(text), i + 2)
            _ => raise UnexpectedToken(tokens[i + 2], [DoubleQuotation])
          }
        _ => raise UnexpectedToken(tokens[i + 2], [DoubleQuotation])
      }
    Token::SingleQuotation =>
      match tokens[i + 1] {
        Token::Text(text) =>
          match tokens[i + 2] {
            SingleQuotation => (Node::String(text), i + 2)
            _ => raise UnexpectedToken(tokens[i + 2], [SingleQuotation])
          }
        _ => raise UnexpectedToken(tokens[i + 2], [SingleQuotation])
      }
    _ => raise UnexpectedToken(tokens[i + 2], [SingleQuotation,DoubleQuotation])
  }
}

test "parse_string"{
assert_eq(parse_string([Token::DoubleQuotation,Token::Text("hoge"),Token::DoubleQuotation],0),(Node::String("hoge"),2))
}

///|
