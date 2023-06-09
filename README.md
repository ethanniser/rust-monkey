monkey interpreter in rust following the book https://interpreterbook.com/

made some additions and modifications from the book:

- added `||`, `&&`, `//` and `%` operators
- reassigment of variables
- `exit` and `env` repl commands
- semicolons are significant: semicolon = statement, statements = `none`. No semicolon = expression, expressions = _a value_
- blocks (`{}`) are expressions, they evaluate to the last expression in the block (or a return statement if there is one), however they do not introduce a new scope
- added _standard library_ of code that is loaded at the start of the repl (map, filter, reduce, sum)
- `type` builtin function to get the type of a value
- probably more stuff I forgot
