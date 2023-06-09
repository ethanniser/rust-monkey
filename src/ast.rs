pub use expression::*;
pub use statement::*;

pub struct Program {
    pub statements: Vec<Statement>,
}

mod statement {

    use super::*;

    #[derive(Debug, PartialEq, Clone)]
    pub enum Statement {
        Let(LetStatement),
        Return(ReturnStatement),
        Expression(ExpressionStatement),
        Assign(AssignStatement),
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct LetStatement {
        pub name: IdentifierLiteral,
        pub value: Expression,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct ReturnStatement {
        pub return_value: Option<Expression>,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum ExpressionStatement {
        Terminating(Expression),
        NonTerminating(Expression),
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct AssignStatement {
        pub name: IdentifierLiteral,
        pub value: Expression,
    }
}

mod expression {

    use std::fmt::Display;

    use super::*;

    #[derive(Debug, PartialEq, Clone)]
    pub enum Expression {
        Ident(IdentifierLiteral),
        Int(IntegerLiteral),
        Bool(BooleanLiteral),
        Fn(FunctionLiteral),
        String(StringLiteral),
        Array(ArrayLiteral),
        Hash(HashLiteral),
        If(IfExpression),
        Call(CallExpression),
        Block(BlockExpression),
        Prefix(PrefixExpression),
        Infix(InfixExpression),
        Index(IndexExpression),
        NoneLiteral,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct IndexExpression {
        pub left: Box<Expression>,
        pub index: Box<Expression>,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct ArrayLiteral {
        pub elements: Vec<Expression>,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct IdentifierLiteral {
        pub value: String,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct IntegerLiteral {
        pub value: isize,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct BooleanLiteral {
        pub value: bool,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct StringLiteral {
        pub value: String,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct FunctionLiteral {
        pub parameters: Vec<IdentifierLiteral>,
        pub body: BlockExpression,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct HashLiteral {
        pub pairs: Vec<(Expression, Expression)>,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct IfExpression {
        pub condition: Box<Expression>,
        pub consequence: BlockExpression,
        pub alternative: Option<BlockExpression>,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct CallExpression {
        pub left: Box<Expression>,
        pub arguments: Vec<Expression>,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct BlockExpression {
        pub statements: Vec<Statement>,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct PrefixExpression {
        pub operator: PrefixOperator,
        pub right: Box<Expression>,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub struct InfixExpression {
        pub left: Box<Expression>,
        pub operator: InfixOperator,
        pub right: Box<Expression>,
    }

    #[derive(Debug, PartialEq, Clone)]
    pub enum PrefixOperator {
        Bang,
        Minus,
    }

    impl Display for PrefixOperator {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                PrefixOperator::Bang => write!(f, "!"),
                PrefixOperator::Minus => write!(f, "-"),
            }
        }
    }

    #[derive(Debug, PartialEq, Copy, Clone)]
    pub enum InfixOperator {
        Plus,
        Minus,
        Asterisk,
        Slash,
        Equal,
        NotEqual,
        LessThan,
        GreaterThan,
        Percent,
        DoubleSlash,
        DoubleAmpersand,
        DoublePipe,
        LParen,
        LBracket,
    }

    impl Display for InfixOperator {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                InfixOperator::Plus => write!(f, "+"),
                InfixOperator::Minus => write!(f, "-"),
                InfixOperator::Asterisk => write!(f, "*"),
                InfixOperator::Slash => write!(f, "/"),
                InfixOperator::Equal => write!(f, "=="),
                InfixOperator::NotEqual => write!(f, "!="),
                InfixOperator::LessThan => write!(f, "<"),
                InfixOperator::GreaterThan => write!(f, ">"),
                InfixOperator::Percent => write!(f, "%"),
                InfixOperator::DoubleSlash => write!(f, "//"),
                InfixOperator::DoubleAmpersand => write!(f, "&&"),
                InfixOperator::DoublePipe => write!(f, "||"),
                InfixOperator::LParen => write!(f, "("),
                InfixOperator::LBracket => write!(f, "["),
            }
        }
    }

    impl Display for FunctionLiteral {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let parameters = self
                .parameters
                .iter()
                .map(|literal| literal.value.clone())
                .collect::<Vec<_>>()
                .join(", ");

            write!(f, "fn({parameters}) -> {}", self.body)
        }
    }

    impl Display for BlockExpression {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let display = match self.statements.last() {
                Some(statement) => match statement {
                    Statement::Expression(expression) => match expression {
                        ExpressionStatement::Terminating(_) => "none".to_string(),
                        ExpressionStatement::NonTerminating(expression) => {
                            format!("{expression}")
                        }
                    },
                    Statement::Return(return_statement) => match return_statement.return_value {
                        Some(ref expression) => format!("{expression}"),
                        None => "none".to_string(),
                    },
                    Statement::Let(_) => "none".to_string(),
                    Statement::Assign(_) => "none".to_string(),
                },
                None => "none".to_string(),
            };

            let dots = if self.statements.len() > 1 { "..." } else { "" };

            let combined = format!("{dots}{display}");

            write!(f, "{combined}")
        }
    }

    impl Display for Expression {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Expression::Ident(identifier) => write!(f, "{}", identifier.value),
                Expression::Int(integer) => write!(f, "{}", integer.value),
                Expression::Bool(boolean) => write!(f, "{}", boolean.value),
                Expression::String(string) => write!(f, "{}", string.value),
                Expression::Fn(function) => write!(f, "{function}"),
                Expression::If(if_expression) => write!(f, "{if_expression}"),
                Expression::Call(call_expression) => write!(f, "{call_expression}"),
                Expression::Block(block_expression) => write!(f, "{block_expression}"),
                Expression::Prefix(prefix_expression) => write!(f, "{prefix_expression}"),
                Expression::Infix(infix_expression) => write!(f, "{infix_expression}"),
                Expression::NoneLiteral => write!(f, "none"),
                Expression::Array(array) => write!(f, "{array}"),
                Expression::Index(index) => write!(f, "{index}"),
                Expression::Hash(hash) => write!(f, "{hash}"),
            }
        }
    }

    impl Display for HashLiteral {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let pairs = self
                .pairs
                .iter()
                .map(|(key, value)| format!("{key}: {value}"))
                .collect::<Vec<_>>()
                .join(", ");

            write!(f, "{{{pairs}}}")
        }
    }

    impl Display for IndexExpression {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}[{}]", self.left, self.index)
        }
    }

    impl Display for ArrayLiteral {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let elements = self
                .elements
                .iter()
                .map(|expression| format!("{expression}"))
                .collect::<Vec<_>>()
                .join(", ");

            write!(f, "[{elements}]")
        }
    }

    impl Display for IfExpression {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let alternative = match &self.alternative {
                Some(expression) => format!("else {expression}"),
                None => "".to_string(),
            };

            write!(
                f,
                "if {} {} {}",
                self.condition, self.consequence, alternative
            )
        }
    }

    impl Display for CallExpression {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let arguments = self
                .arguments
                .iter()
                .map(|expression| format!("{expression}"))
                .collect::<Vec<_>>()
                .join(", ");

            write!(f, "{}({})", self.left, arguments)
        }
    }

    impl Display for PrefixExpression {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}{}", self.operator, self.right)
        }
    }

    impl Display for InfixExpression {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{} {} {}", self.left, self.operator, self.right)
        }
    }
}
