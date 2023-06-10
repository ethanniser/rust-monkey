use super::code::Instructions;
use crate::{ast::Program, object::Object};

pub struct Compiler {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

pub struct Bytecode {
    pub instructions: Instructions,
    pub constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Self {
        Compiler {
            instructions: Instructions { data: Vec::new() },
            constants: Vec::new(),
        }
    }

    pub fn compile(&mut self, input: Program) -> Result<(), String> {
        todo!()
    }

    pub fn bytecode(&self) -> Bytecode {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use crate::{compiler::code::*, lexer::Lexer, parser::Parser};

    use super::*;

    fn parse(input: &str) -> Program {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        assert!(parser.errors.len() == 0);
        program
    }

    fn test_compiler(
        input: &str,
        expected_constants: Vec<Object>,
        expected_instructions: Vec<Instructions>,
    ) {
        let program = parse(input);
        let mut compiler = Compiler::new();
        compiler
            .compile(program)
            .unwrap_or_else(|err| panic!("compilier faile to compile: {err}"));

        let bytecode = compiler.bytecode();

        test_instructions(expected_instructions, bytecode.instructions);
        test_constants(expected_constants, bytecode.constants);
    }

    fn test_constants(expected: Vec<Object>, actual: Vec<Object>) {
        assert_eq!(expected.len(), actual.len());

        for (expected, actual) in expected.iter().zip(actual.iter()) {
            match (expected, actual) {
                (Object::Integer(expected), Object::Integer(actual)) => {
                    assert_eq!(expected, actual);
                }
                _ => {
                    panic!("unhandled object type");
                }
            }
        }
    }

    fn test_instructions(expected: Vec<Instructions>, actual: Instructions) {
        let flattened = expected
            .iter()
            .map(|x| x.data.clone())
            .flatten()
            .collect::<Vec<u8>>();

        assert_eq!(flattened.len(), actual.data.len());
        assert_eq!(flattened, actual.data);
    }

    #[test]
    fn interger_arithmetic() {
        let input = "1 + 2";
        let expected_constants = vec![Object::Integer(1), Object::Integer(2)];
        let expected_instructions =
            vec![make(OpCode::Constant, &[0]), make(OpCode::Constant, &[1])];

        test_compiler(input, expected_constants, expected_instructions);
    }
}
