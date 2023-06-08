use crate::environment::Environment;
use crate::object::Object;
use crate::parser::Parser;
use crate::{evaluator::Node, lexer::Lexer};
use std::cell::RefCell;
use std::io::{self, BufRead, Write};
use std::rc::Rc;

const PROMPT: &str = ">> ";

const MONKEY_FACE: &str = r#"         .-"-.
       _/_-.-_\_
      /|( o o )|\
     / //  "  \\ \ 
    / / \'---'/ \ \
    \ \_/`"""`\_/ /
     \           /"#;

pub fn start<R: BufRead, W: Write + 'static>(input: R, mut output: W) -> io::Result<()> {
    writeln!(
        output,
        "Hello! This is the Monkey programming language, with some *personal modifications*"
    )?;
    writeln!(
        output,
        "Feel free to type in commands, and simply type \"exit\" to exit the REPL."
    )?;

    let output = Rc::new(RefCell::new(output));
    let mut lines = input.lines();
    let ref env = Environment::new_with_output(Rc::clone(&output));

    // debug_scope(env, 0);

    // println!("{:?}", (*env).borrow().store);

    loop {
        let mut temp_output = output.borrow_mut();
        let buffer = &mut *temp_output;

        write!(buffer, "{PROMPT}")?;
        buffer.flush()?;

        let line = match lines.next() {
            Some(Ok(line)) => line,
            Some(Err(e)) => return Err(e),
            None => return Ok(()),
        };

        if line == "exit" {
            return Ok(());
        }

        let lexer = Lexer::new(line);

        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if !parser.errors.is_empty() {
            writeln!(buffer, "Woops! We ran into some monkey business here!")?;
            writeln!(buffer, "{}", MONKEY_FACE)?;
            writeln!(buffer, "Parser Error:")?;

            for error in parser.errors {
                writeln!(buffer, "{}", error)?;
            }
        }

        drop(buffer);
        drop(temp_output);

        // writeln!(output, "<temp> parser output{:?}", program.statements)?;

        let result = program.eval(env);

        let mut temp_output = output.borrow_mut();
        let buffer = &mut *temp_output;

        let evaluated = match result {
            Ok(evaluated) => evaluated,
            Err(e) => {
                writeln!(buffer, "Woops! We ran into some monkey business here!")?;
                writeln!(buffer, "{}", MONKEY_FACE)?;
                writeln!(buffer, "Evaluation Error:")?;
                writeln!(buffer, "{e}")?;
                continue;
            }
        };

        // debug_scope(env, 0);

        if let Object::None = *evaluated {
            continue;
        }

        writeln!(buffer, "{:?}", evaluated)?
    }
}
