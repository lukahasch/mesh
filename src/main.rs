use mesh::parser::lib::Source;
use nom::{Parser, combinator::eof};

fn main() {
    let file = "example.mesh";
    let input = std::fs::read_to_string(file).unwrap();
    let source = Source::new(file, &input);
    let result = mesh::parser::parser.parse_complete(source);
    match result {
        Ok((_, ast)) => println!("{:#?}", ast),
        Err(nom::Err::Failure(error)) | Err(nom::Err::Error(error)) => {
            error.report().eprint(error.cache()).unwrap();
        }
        Err(nom::Err::Incomplete(_)) => unreachable!(),
    }
}
