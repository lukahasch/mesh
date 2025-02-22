pub trait Expression {
    type Output;
    fn parse(&self, input: &str) -> Option<(&str, Self::Output)>;
    fn generate(&self) -> String;
}

pub struct Word(String);
