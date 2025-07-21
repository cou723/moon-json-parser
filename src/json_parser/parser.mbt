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
suberror ParserErrors {
  UnexpectedToken(Token, Array[TokenKind]) // Actual, Expects
  UnexpectedTokens(Array[Token], Array[TokenKind]) // Actual, Expects
  UnexpectedTerminate(Array[TokenKind]) // Expects
  UnexpectedExtraToken(Array[Token]) // Extra Token
} derive(Eq, Show)

///|
suberror JsonParserError {
  ParserError(ParserErrors)
  LexerError(LexerErrors)
} derive(Eq, Show)

///|
pub fn from_string(text : String) -> Node raise JsonParserError {
  match (try? lex(text).0) {
    Ok(tokens) =>
      match (try? parse(tokens)) {
        Ok(parsed) => parsed
        Err(e) => raise JsonParserError::ParserError(e)
      }
    Err(e) => raise JsonParserError::LexerError(e)
  }
}

///|
pub fn parse(tokens : Array[Token]) -> Node raise ParserErrors {
  let result = parse_json(tokens)
  if result.1.length() != 0 {
    raise UnexpectedExtraToken(result.1)
  }
  return result.0
}

///|
pub fn parse_json(
  tokens : Array[Token]
) -> (Node, Array[Token]) raise ParserErrors {
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

///|
fn parse_key_values(
  tokens : Array[Token]
) -> (Node, Array[Token]) raise ParserErrors {
  loop ([], tokens) {
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
            BraceEnd => break (Object(loop_kvs), loop_tokens)
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
              break (Object(attribute_consumed_kvs), attribute_consumed_tokens)
            _ => raise UnexpectedToken(next_token, [Comma, BracketEnd])
          }
      }
    }
  }
}

///|
fn parse_object(
  tokens : Array[Token]
) -> (Node, Array[Token]) raise ParserErrors {
  let (node, array) = match tokens.get(0) {
    None => raise UnexpectedTerminate([BraceStart])
    Some(token) =>
      match token {
        BraceStart => parse_key_values(tokens[1:].to_array())
        _ => raise UnexpectedToken(token, [BraceStart])
      }
  }
  match array.get(0).map(fn(t) { t }) {
    Some(BraceEnd) => (node, array[1:].to_array())
    _ => raise UnexpectedToken(array[0], [BraceEnd])
  }
}

///|
fn _parse_key_value(
  tokens : Array[Token]
) -> ((String, Node), Array[Token]) raise ParserErrors {
  let (key_text, key_loaded_tokens) = match tokens.get(0) {
    None => raise UnexpectedTerminate([SingleQuotation, DoubleQuotation])
    Some(t) =>
      match t {
        SingleQuotation | DoubleQuotation =>
          match parse_string(tokens) {
            (String(s), array) => (s, array)
            _ => raise UnexpectedToken(t, [Text])
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
  let (value_node, value_loaded_tokens) = parse_json(colon_consumed_tokens)
  ((key_text, value_node), value_loaded_tokens)
}

///|
fn parse_array(
  tokens : Array[Token]
) -> (Node, Array[Token]) raise ParserErrors {
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
fn unexpected_terminate_array_element() -> ParserErrors {
  UnexpectedTerminate([
    BracketStart,
    SingleQuotation,
    DoubleQuotation,
    Number,
    Decimal,
    Boolean,
    Null,
  ])
}

///|
fn parse_array_element(
  tokens : Array[Token]
) -> (Array[Node], Array[Token]) raise ParserErrors {
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
            _ => raise UnexpectedToken(token, [BracketStart])
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
) -> (Node, Array[Token]) raise ParserErrors {
  match tokens.map(fn(t) { t }) {
    [DoubleQuotation, Text(text), DoubleQuotation, .. rest] =>
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
      Token::DoubleQuotation(pos=0),
      Token::Text("hoge", pos=1),
      Token::DoubleQuotation(pos=5),
    ]),
    (Node::String("hoge"), []),
  )
}

///|
test "parse_string with extra tokens" {
  assert_eq(
    parse_string([
      Token::DoubleQuotation(pos=0),
      Token::Text("hoge", pos=1),
      Token::DoubleQuotation(pos=5),
      Token::Colon(pos=6),
      Token::Null(pos=7),
      Token::Null(pos=8),
    ]),
    (
      Node::String("hoge"),
      [Token::Colon(pos=6), Token::Null(pos=7), Token::Null(pos=8)],
    ),
  )
}

///|
test "parse_array empty" {
  assert_eq(
    parse_array([BracketStart(pos=0), BracketEnd(pos=1)]),
    (Array([]), []),
  )
}

///|
test "parse_array single element" {
  assert_eq(
    parse_array([
      BracketStart(pos=0),
      SingleQuotation(pos=1),
      Text("string", pos=2),
      SingleQuotation(pos=8),
      BracketEnd(pos=9),
    ]),
    (Array([String("string")]), []),
  )
}

///|
test "parse_array multiple element" {
  assert_eq(
    parse_array([
      BracketStart(pos=0),
      SingleQuotation(pos=1),
      Text("string1", pos=2),
      SingleQuotation(pos=9),
      Comma(pos=10),
      SingleQuotation(pos=11),
      Text("string2", pos=12),
      SingleQuotation(pos=19),
      BracketEnd(pos=20),
    ]),
    (Array([String("string1"), String("string2")]), []),
  )
}

///|
test "parse_array with extra tokens" {
  assert_eq(
    parse_array([
      BracketStart(pos=0),
      SingleQuotation(pos=1),
      Text("string", pos=2),
      SingleQuotation(pos=8),
      BracketEnd(pos=9),
      Null(pos=10),
    ]),
    (Array([String("string")]), [Null(pos=10)]),
  )
}

///|
test "parse_key_value normal" {
  assert_eq(
    _parse_key_value([
      SingleQuotation(pos=0),
      Text("key", pos=0),
      SingleQuotation(pos=0),
      Colon(pos=0),
      SingleQuotation(pos=0),
      Text("value", pos=0),
      SingleQuotation(pos=0),
    ]),
    (("key", String("value")), []),
  )
}

///|
test "parse_key_value extend tokens" {
  assert_eq(
    _parse_key_value([
      SingleQuotation(pos=0),
      Text("key", pos=0),
      SingleQuotation(pos=0),
      Colon(pos=0),
      SingleQuotation(pos=0),
      Text("value", pos=0),
      SingleQuotation(pos=0),
      Token::Null(pos=0),
      Token::Null(pos=0),
      Token::Null(pos=0),
    ]),
    (
      ("key", String("value")),
      [Token::Null(pos=0), Token::Null(pos=0), Token::Null(pos=0)],
    ),
  )
}

///|
test "parse_key_values single attribute" {
  assert_eq(
    parse_key_values([
      SingleQuotation(pos=0),
      Text("key", pos=0),
      SingleQuotation(pos=0),
      Colon(pos=0),
      SingleQuotation(pos=0),
      Text("value", pos=0),
      SingleQuotation(pos=0),
      BraceEnd(pos=0),
    ]),
    (Object([("key", String("value"))]), [BraceEnd(pos=0)]),
  )
}

///|
test "parse_key_values multiple attributes" {
  assert_eq(
    parse_key_values([
      // "key":"value"
      SingleQuotation(pos=0),
      Text("key", pos=0),
      SingleQuotation(pos=0),
      Colon(pos=0),
      SingleQuotation(pos=0),
      Text("value", pos=0),
      SingleQuotation(pos=0),
      // (pos=0),
      Comma(pos=0),
      // "key":"value"
      SingleQuotation(pos=0),
      Text("key", pos=0),
      SingleQuotation(pos=0),
      Colon(pos=0),
      SingleQuotation(pos=0),
      Text("value", pos=0),
      SingleQuotation(pos=0),
      BraceEnd(pos=0),
    ]),
    (
      Object([("key", String("value")), ("key", String("value"))]),
      [BraceEnd(pos=0)],
    ),
  )
}

///|
test "parse_object minimum" {
  assert_eq(
    parse_object([BraceStart(pos=0), BraceEnd(pos=1)]),
    (Object([]), []),
  )
}

///|
test "parse all" {
  assert_eq(
    parse_object([
      BraceStart(pos=0),
      // "string":"this is string"
      SingleQuotation(pos=0),
      Text("string", pos=0),
      SingleQuotation(pos=0),
      Colon(pos=0),
      SingleQuotation(pos=0),
      Text("this is string", pos=0),
      SingleQuotation(pos=0),
      Comma(pos=0),
      // "number":1
      SingleQuotation(pos=0),
      Text("number", pos=0),
      SingleQuotation(pos=0),
      Colon(pos=0),
      Number(1, pos=0),
      Comma(pos=0),
      // "null": null
      SingleQuotation(pos=0),
      Text("null", pos=0),
      SingleQuotation(pos=0),
      Colon(pos=0),
      Null(pos=0),
      Comma(pos=0),
      // "boolean":true
      SingleQuotation(pos=0),
      Text("boolean", pos=0),
      SingleQuotation(pos=0),
      Colon(pos=0),
      Boolean(true, pos=0),
      Comma(pos=0),
      // "Decimal":3.14
      SingleQuotation(pos=0),
      Text("decimal", pos=0),
      SingleQuotation(pos=0),
      Colon(pos=0),
      Decimal(3.14, pos=0),
      Comma(pos=0),
      // "array": [0,1,2]
      SingleQuotation(pos=0),
      Text("array", pos=0),
      SingleQuotation(pos=0),
      Colon(pos=0),
      BracketStart(pos=0),
      Number(0, pos=0),
      Comma(pos=0),
      Number(1, pos=0),
      Comma(pos=0),
      Number(2, pos=0),
      BracketEnd(pos=0),
      Comma(pos=0),
      // "object": {"value":"key"}
      SingleQuotation(pos=0),
      Text("object", pos=0),
      SingleQuotation(pos=0),
      Colon(pos=0),
      BraceStart(pos=0),
      SingleQuotation(pos=0),
      Text("value", pos=0),
      SingleQuotation(pos=0),
      Colon(pos=0),
      SingleQuotation(pos=0),
      Text("key", pos=0),
      SingleQuotation(pos=0),
      BraceEnd(pos=0),
      BraceEnd(pos=0),
    ]),
    (
      Object([
        ("string", String("this is string")),
        ("number", Number(1)),
        ("null", Null),
        ("boolean", Boolean(true)),
        ("decimal", Decimal(3.14)),
        ("array", Array([Number(0), Number(1), Number(2)])),
        ("object", Object([("value", String("key"))])),
      ]),
      [],
    ),
  )
}
