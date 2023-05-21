use crate::lexer::Lexer;
use crate::token::Token;
use std::io::{self, BufRead, Write};

const PROMPT: &str = ">> ";

pub fn start<R: BufRead, W: Write>(input: R, mut output: W) -> io::Result<()> {
    let mut lines = input.lines();

    loop {
        write!(output, "{}", PROMPT)?;
        output.flush()?;

        let line = match lines.next() {
            Some(Ok(line)) => line,
            Some(Err(e)) => return Err(e),
            None => return Ok(()),
        };

        let lexer = Lexer::new(line);

        let tokens: Vec<Token> = lexer.into_iter().collect();
        writeln!(output, "{:?}", tokens)?;
    }
}
