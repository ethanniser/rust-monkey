use monkey::*;

use std::io;

fn main() -> io::Result<()> {
    let input = io::stdin();
    let output = io::stdout();

    repl::start(input.lock(), output.lock())
}
