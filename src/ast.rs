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
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Temp(Temp),
    Int(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
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

#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub return_value: Expression,
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expression,
}

#[derive(Debug, PartialEq)]
pub struct IntegerLiteral {
    pub value: isize,
}

#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    pub operator: PrefixOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq)]
pub enum PrefixOperator {
    Bang,
    Minus,
}

#[derive(Debug, PartialEq)]
pub enum InfixOperator {
    Plus,
    Minus,
    Asterisk,
    Slash,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
}

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    pub left: Box<Expression>,
    pub operator: InfixOperator,
    pub right: Box<Expression>,
}
