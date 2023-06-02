monkey interpreter in rust following the book https://interpreterbook.com/

my implementation's changes:

no nulls!
none is only to represent an expression that doesn't return a value
all other instances of null in the book are errors
