pub struct Program {
    pub statements: Vec<Statement>,
}

pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Temp(Temp),
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub value: String,
}

#[derive(Debug, PartialEq)]
pub struct Temp;
