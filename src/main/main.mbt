///|
fn main {
  try {
    let json_raw_text = @fs.read_file_to_string("./moon.mod.json")
    println(try? @json_parser.from_string(json_raw_text))
  } catch {
    _ => println("error")
  }
}
