use mesh::parser::lib::Source;
use nom::{Parser, combinator::eof};

fn main() {
    let file = "example.mesh";
    let input = std::fs::read_to_string(file).unwrap();
    let source = Source::new(file, &input);
    let (_, (result, _)) = mesh::parser::program
        .and(eof)
        .parse_complete(source)
        .unwrap();
    for node in result {
        println!("{:?}", node);
    }
}
