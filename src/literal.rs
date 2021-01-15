#[derive(Clone, Debug)]
pub enum Literal {
    String(String),
    Number(f64),
    Bool(bool),
    Nil,
}
