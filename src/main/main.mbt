///|
fn main {
  try {
    let hoge = @fs.read_file_to_string!("./test.json")
    println(@json_parser.lex!(hoge))
  } catch {
    _ => println("error")
  }
}
