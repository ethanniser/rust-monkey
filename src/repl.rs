use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{self, BufRead, Write};

const PROMPT: &str = ">> ";

pub fn start<R: BufRead, W: Write>(input: R, mut output: W) -> io::Result<()> {
    let mut lines = input.lines();

    loop {
        write!(output, "{PROMPT}")?;
        output.flush()?;

        let line = match lines.next() {
            Some(Ok(line)) => line,
            Some(Err(e)) => return Err(e),
            None => return Ok(()),
        };

        let lexer = Lexer::new(line);

        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if !parser.errors.is_empty() {
            writeln!(output, "ERRORS:")?;

            for error in parser.errors {
                writeln!(output, "{}", error.message)?;
            }
            continue;
        }

        writeln!(output, "PARSER OUTPUT:")?;
        writeln!(output, "{:?}", program.statements)?;
    }
}
