enum Node {
  Object(Array[(String, Node)])
  String(String)
  Number(Int)
  Decimal(Double)
  Array(Array[Node])
  Null
  Boolean(Bool)
} derive(Eq, Show)

suberror ParserError {
  UnexpectedToken(Token, Array[Token]) // Actual, Expects
  UnexpectedTokens(Array[Token], Array[Token]) // Actual, Expects
  UnexpectedTerminate(Array[Token]) // Expects
} derive(Eq, Show)

pub fn parse(tokens : Array[Token]) -> (Node, Array[Token]) raise ParserError {
  match tokens[0] {
    SingleQuotation | DoubleQuotation => parse_string(tokens)
    BracketStart => parse_array(tokens)
    BraceStart => parse_object(tokens)
    Null => (Null, tokens[1:].to_array())
    Boolean(b) => (Boolean(b), tokens[1:].to_array())
    Decimal(d) => (Decimal(d), tokens[1:].to_array())
    Number(n) => (Number(n), tokens[1:].to_array())
    _ =>
      raise UnexpectedToken(tokens[0], [
        SingleQuotation,
        DoubleQuotation,
        BracketStart,
        BraceStart,
        Null,
      ])
  }
}
fn parse_key_values(
  tokens : Array[Token]
) -> (Node, Array[Token]) raise ParserError {
  loop ([], tokens[1:].to_array()) {
    (loop_kvs, loop_tokens) => {
      let (attribute_consumed_kvs, attribute_consumed_tokens) = match
        loop_tokens.get(0) {
        None =>
          raise UnexpectedTerminate([SingleQuotation, DoubleQuotation, BraceEnd])
        Some(t) =>
          match t {
            SingleQuotation | DoubleQuotation => {
              let (_key_value, _tokens) = _parse_key_value(loop_tokens)
              ([..loop_kvs, _key_value], _tokens)
            }
            BraceEnd => break (Object(loop_kvs), loop_tokens[1:].to_array())
            _ => raise UnexpectedToken(t, [BraceStart])
          }
      }
      match attribute_consumed_tokens.get(0) {
        None => raise UnexpectedTerminate([BraceEnd])
        Some(next_token) =>
          match next_token {
            Comma =>
              continue (
                  attribute_consumed_kvs,
                  attribute_consumed_tokens[1:].to_array(),
                )
            BraceEnd =>
              break (
                Object(attribute_consumed_kvs),
                attribute_consumed_tokens[1:].to_array(),
              )
            _ => raise UnexpectedToken(next_token, [Comma, BracketEnd])
          }
      }
    }
  }
}

///|
fn parse_object(
  tokens : Array[Token]
) -> (Node, Array[Token]) raise ParserError {
  match tokens.get(0) {
    None => raise UnexpectedTerminate([BraceStart])
    Some(token) =>
      match token {
        BraceStart => parse_key_values(tokens[1:].to_array())
        _ => raise UnexpectedToken(token, [BraceStart])
      }
  }
}

///|
fn _parse_key_value(
  tokens : Array[Token]
) -> ((String, Node), Array[Token]) raise ParserError {
  let (key_text, key_loaded_tokens) = match tokens.get(0) {
    None => raise UnexpectedTerminate([SingleQuotation, DoubleQuotation])
    Some(t) =>
      match t {
        SingleQuotation | DoubleQuotation =>
          match parse_string(tokens) {
            (String(s), array) => (s, array)
            _ => raise UnexpectedToken(t, [Text("")])
          }
        _ => raise UnexpectedToken(t, [SingleQuotation, DoubleQuotation])
      }
  }
  let colon_consumed_tokens = match key_loaded_tokens.get(0) {
    None => raise UnexpectedTerminate([Colon])
    Some(t) =>
      match t {
        Colon => key_loaded_tokens[1:].to_array()
        _ => raise UnexpectedToken(t, [Colon])
      }
  }
  let (value_node, value_loaded_tokens) = parse(colon_consumed_tokens)
  ((key_text, value_node), value_loaded_tokens)
}

///|
fn parse_array(tokens : Array[Token]) -> (Node, Array[Token]) raise ParserError {
  let (nodes, updated_tokens) = match tokens.get(0) {
    None => raise UnexpectedTerminate([BracketStart])
    Some(token) =>
      match token {
        BracketStart => parse_array_element(tokens[1:].to_array())
        _ => raise UnexpectedToken(token, [BracketStart])
      }
  }
  (Node::Array(nodes), updated_tokens)
}

///|
fn unexpected_terminate_array_element() -> ParserError {
  UnexpectedTerminate([
    BracketStart,
    SingleQuotation,
    DoubleQuotation,
    Number(0),
    Decimal(0),
    Boolean(true),
    Boolean(false),
    Null,
  ])
}

///|
fn parse_array_element(
  tokens : Array[Token]
) -> (Array[Node], Array[Token]) raise ParserError {
  loop ([], tokens) {
    (nodes, tokens) => {
      let (additional_node, _updated_tokens) = match tokens.get(0) {
        None => raise unexpected_terminate_array_element()
        Some(token) =>
          match token {
            SingleQuotation | DoubleQuotation => parse_string(tokens)
            Boolean(b) => (Node::Boolean(b), tokens[1:].to_array())
            Decimal(d) => (Node::Decimal(d), tokens[1:].to_array())
            Number(n) => (Node::Number(n), tokens[1:].to_array())
            Null => (Node::Null, tokens[1:].to_array())
            BracketEnd => break (nodes, tokens[1:].to_array())
            token => raise UnexpectedToken(token, [BracketStart])
          }
      }
      match _updated_tokens.get(0) {
        None => raise UnexpectedTerminate([BracketStart])
        Some(next_token) =>
          match next_token {
            Comma =>
              continue (
                  [..nodes, additional_node],
                  _updated_tokens[1:].to_array(),
                )
            BracketEnd =>
              break ([..nodes, additional_node], _updated_tokens[1:].to_array())
            _ => raise UnexpectedToken(next_token, [Comma, BracketEnd])
          }
      }
    }
  }
}

///|
fn parse_string(
  tokens : Array[Token]
) -> (Node, Array[Token]) raise ParserError {
  match tokens {
    [Token::DoubleQuotation, Token::Text(text), Token::DoubleQuotation, .. rest] =>
      (Node::String(text), rest.to_array())
    [Token::SingleQuotation, Token::Text(text), Token::SingleQuotation, .. rest] =>
      (Node::String(text), rest.to_array())
    [Token::DoubleQuotation, Token::Text(_), actual, ..] =>
      raise UnexpectedToken(actual, [DoubleQuotation])
    [Token::SingleQuotation, Token::Text(_), actual, ..] =>
      raise UnexpectedToken(actual, [SingleQuotation])
    [Token::DoubleQuotation, actual, ..] =>
      raise UnexpectedToken(actual, [DoubleQuotation])
    [Token::SingleQuotation, actual, ..] =>
      raise UnexpectedToken(actual, [SingleQuotation])
    [actual, ..] =>
      raise UnexpectedToken(actual, [SingleQuotation, DoubleQuotation])
    [] => raise UnexpectedTokens(tokens, [SingleQuotation, DoubleQuotation]) // 空配列対応
  }
}

///|
test "parse_string" {
  assert_eq(
    parse_string([
      Token::DoubleQuotation,
      Token::Text("hoge"),
      Token::DoubleQuotation,
    ]),
    (Node::String("hoge"), []),
  )
}

///|
test "parse_string with extra tokens" {
  assert_eq(
    parse_string([
      Token::DoubleQuotation,
      Token::Text("hoge"),
      Token::DoubleQuotation,
      Token::Colon,
      Token::Null,
      Token::Null,
    ]),
    (Node::String("hoge"), [Token::Colon, Token::Null, Token::Null]),
  )
}

///|
test "parse_array empty" {
  assert_eq(parse_array([BracketStart, BracketEnd]), (Array([]), []))
}

///|
test "parse_array single element" {
  assert_eq(
    parse_array([
      BracketStart,
      SingleQuotation,
      Text("string"),
      SingleQuotation,
      BracketEnd,
    ]),
    (Array([String("string")]), []),
  )
}

///|
test "parse_array multiple element" {
  assert_eq(
    parse_array([
      BracketStart,
      SingleQuotation,
      Text("string1"),
      SingleQuotation,
      Comma,
      SingleQuotation,
      Text("string2"),
      SingleQuotation,
      BracketEnd,
    ]),
    (Array([String("string1"), String("string2")]), []),
  )
}

///|
test "parse_array with extra tokens" {
  assert_eq(
    parse_array([
      BracketStart,
      SingleQuotation,
      Text("string"),
      SingleQuotation,
      BracketEnd,
      Null,
    ]),
    (Array([String("string")]), [Null]),
  )
}

///|
test "parse_key_value normal" {
  assert_eq(
    _parse_key_value([
      SingleQuotation,
      Text("key"),
      SingleQuotation,
      Colon,
      SingleQuotation,
      Text("value"),
      SingleQuotation,
    ]),
    (("key", String("value")), []),
  )
}

///|
test "parse_key_value extend tokens" {
  assert_eq(
    _parse_key_value([
      SingleQuotation,
      Text("key"),
      SingleQuotation,
      Colon,
      SingleQuotation,
      Text("value"),
      SingleQuotation,
      Token::Null,
      Token::Null,
      Token::Null,
    ]),
    (("key", String("value")), [Token::Null, Token::Null, Token::Null]),
  )
}

///|
test "parse_key_values single attribute" {
  assert_eq(
    parse_key_values([
      BraceStart,
      SingleQuotation,
      Text("key"),
      SingleQuotation,
      Colon,
      SingleQuotation,
      Text("value"),
      SingleQuotation,
      BraceEnd,
    ]),
    (Object([("key", String("value"))]), []),
  )
}

///|
test "parse_key_values multiple attributes" {
  assert_eq(
    parse_key_values([
      BraceStart,
      // "key":"value"
      SingleQuotation,
      Text("key"),
      SingleQuotation,
      Colon,
      SingleQuotation,
      Text("value"),
      SingleQuotation,
      // ,
      Comma,
      // "key":"value"
      SingleQuotation,
      Text("key"),
      SingleQuotation,
      Colon,
      SingleQuotation,
      Text("value"),
      SingleQuotation,
      BraceEnd,
    ]),
    (Object([("key", String("value")), ("key", String("value"))]), []),
  )
}

///|
// test "parse_object empty" {
//   assert_eq(parse_object([BraceStart, BraceEnd]), (Object([]), []))
// }

// ///|
// test "parse_object empty" {
//   assert_eq(
//     parse_object([
//       BraceStart,
//       SingleQuotation,
//       Text("key"),
//       SingleQuotation,
//       Comma,
//       SingleQuotation,
//       Text("value"),
//       SingleQuotation,
//       BraceEnd,
//     ]),
//     (Object([("key", String("value"))]), []),
//   )
// }
