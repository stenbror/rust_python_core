mod parser;

fn main() {

    println!("Rust Python Core - (C) 2025 Richard Magnor Stenbro.");

    let _ = parser::lexer::PythonCoreLexer::new("1.34e-54J").tokenize_source();
}
