use crate::parser::Parser;
use crate::{evaluator::eval_program, lexer::Lexer};
use std::io::{self, BufRead, Write};

const PROMPT: &str = ">> ";

const MONKEY_FACE: &str = r#"         .-"-.
       _/_-.-_\_
      /|( o o )|\
     / //  "  \\ \ 
    / / \'---'/ \ \
    \ \_/`"""`\_/ /
     \           /"#;

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
            writeln!(output, "Woops! We ran into some monkey business here!")?;
            writeln!(output, "{}", MONKEY_FACE)?;
            writeln!(output, "PARSER ERRORS:")?;

            for error in parser.errors {
                writeln!(output, "{}", error.message)?;
            }
        }

        let evaluated = match eval_program(&program) {
            Ok(evaluated) => evaluated,
            Err(_e) => {
                writeln!(output, "Woops! We ran into some monkey business here!")?;
                writeln!(output, "{}", MONKEY_FACE)?;
                writeln!(output, "EVAL ERRORS:")?;
                writeln!(output, "{}", "there was an error <this should be fixed>")?;
                continue;
            }
        };

        match evaluated {
            Some(evaluated) => writeln!(output, "{}", evaluated)?,
            None => writeln!(output, "")?,
        }
    }
}
