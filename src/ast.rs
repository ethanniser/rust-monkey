pub use expression::*;
pub use statement::*;

pub struct Program {
    pub statements: Vec<Statement>,
}

pub enum Node {
    Statement(Statement),
    Expression(Expression),
}

mod statement {

    use super::*;

    #[derive(Debug, PartialEq)]
    pub enum Statement {
        Let(LetStatement),
        Return(ReturnStatement),
        Expression(ExpressionStatement),
        Block(BlockStatement),
    }
    #[derive(Debug, PartialEq)]
    pub struct LetStatement {
        pub name: IdentifierLiteral,
        pub value: Expression,
    }

    #[derive(Debug, PartialEq)]
    pub struct ReturnStatement {
        pub return_value: Expression,
    }

    #[derive(Debug, PartialEq)]
    pub struct ExpressionStatement {
        pub expression: Expression,
    }

    #[derive(Debug, PartialEq)]
    pub struct BlockStatement {
        pub statements: Vec<Statement>,
    }
}

mod expression {

    use super::*;

    #[derive(Debug, PartialEq)]
    pub enum Expression {
        Identifier(IdentifierLiteral),
        Int(IntegerLiteral),
        Boolean(BooleanLiteral),
        Function(FunctionLiteral),
        If(IfExpression),
        Prefix(PrefixExpression),
        Infix(InfixExpression),
    }

    #[derive(Debug, PartialEq)]
    pub struct IdentifierLiteral {
        pub value: String,
    }

    #[derive(Debug, PartialEq)]
    pub struct IntegerLiteral {
        pub value: isize,
    }

    #[derive(Debug, PartialEq)]
    pub struct BooleanLiteral {
        pub value: bool,
    }

    #[derive(Debug, PartialEq)]
    pub struct FunctionLiteral {
        pub parameters: Vec<IdentifierLiteral>,
        pub body: BlockStatement,
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

    #[derive(Debug, PartialEq)]
    pub struct IfExpression {
        pub condition: Box<Expression>,
        pub consequence: BlockStatement,
        pub alternative: Option<BlockStatement>,
    }
}
