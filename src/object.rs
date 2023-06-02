#[derive(Debug, PartialEq)]
pub enum Object {
    Integer(isize),
    Boolean(bool),
    Null,
}
