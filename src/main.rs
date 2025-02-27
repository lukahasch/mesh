use mesh::parser::lib::Source;

fn main() {
    let file = "example.mesh";
    let input = std::fs::read_to_string(file).unwrap();
    let source = Source::new(file, &input);
    let result = mesh::parser::parse(source);
    match result {
        Ok(nodes) => {
            for node in nodes {
                println!("{:?}", node);
            }
        }
        Err(e) => {
            e.report().eprint(e.cache()).unwrap();
        }
    };
}
