use crate::environment::Env;
use crate::evaluator::Node;
use crate::lexer::Lexer;
use crate::object::Object;
use crate::parser::Parser;
use std::fmt::Display;
use std::{fmt::Formatter, rc::Rc};

pub type BuiltInFunctionType = fn(Vec<Rc<Object>>) -> Result<Rc<Object>, BuiltInFunctionError>;

#[derive(Debug, PartialEq)]
pub struct BuiltInFunctionError {
    pub function: BuiltInFunction,
    pub error: BIFInnerError,
    pub message: Option<String>,
}

impl Display for BuiltInFunctionError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let preface = format!("Error in built in function '{}': ", self.function);
        let error = match &self.error {
            BIFInnerError::WrongNumberOfArguments { expected, got } => format!(
                "Wrong number of arguments. Expected {}, got {}",
                expected, got
            ),
            BIFInnerError::WrongArgumentType { expected, got } => {
                format!(
                    "Wrong argument type. Expected: {}, Got: {}",
                    expected,
                    got.to_type()
                )
            }
        };
        let message = match self.message.clone() {
            Some(message) => format!("\n{}", message),
            None => "".to_string(),
        };

        write!(f, "{}{}{}", preface, error, message)
    }
}

#[derive(Debug, PartialEq)]
pub enum BIFInnerError {
    WrongNumberOfArguments {
        expected: usize,
        got: usize,
    },
    WrongArgumentType {
        expected: ObjectExpectation,
        got: Rc<Object>,
    },
}

#[derive(Debug, PartialEq)]
pub enum ObjectExpectation {
    One(Rc<Object>),
    Many(Vec<Rc<Object>>),
}

impl Display for ObjectExpectation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ObjectExpectation::One(object) => write!(f, "{}", object.to_type()),
            ObjectExpectation::Many(objects) => {
                let type_union = objects
                    .iter()
                    .map(|o| o.to_type())
                    .collect::<Vec<_>>()
                    .join(" | ");
                write!(f, "{}", type_union)
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct BuiltInFunction {
    pub function: BuiltInFunctionType,
    type_signature: &'static str,
}

impl Display for BuiltInFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_signature)
    }
}

pub struct BuiltIns {
    pub functions: Vec<(String, BuiltInFunction)>,
}

mod type_signatures {
    pub const LEN: &str = "len(x: string | array) -> integer";
    pub const FIRST: &str = "first(x: array) -> any | none";
    pub const LAST: &str = "last(x: array) -> any | none";
    pub const REST: &str = "rest(x: array) -> array | none";
    pub const PUSH: &str = "push(x: array, y: any) -> array";
}

pub const BUILT_IN_FUNCTIONS: [(&'static str, BuiltInFunction); 5] = [
    (
        "len",
        BuiltInFunction {
            function: len,
            type_signature: type_signatures::LEN,
        },
    ),
    (
        "first",
        BuiltInFunction {
            function: first,
            type_signature: type_signatures::FIRST,
        },
    ),
    (
        "last",
        BuiltInFunction {
            function: last,
            type_signature: type_signatures::LAST,
        },
    ),
    (
        "rest",
        BuiltInFunction {
            function: rest,
            type_signature: type_signatures::REST,
        },
    ),
    (
        "push",
        BuiltInFunction {
            function: push,
            type_signature: type_signatures::PUSH,
        },
    ),
];

const STD_LIB: &str = "
let map = fn(arr, f) {
  let iter = fn(arr, accumulated) {
    if (len(arr) == 0) {
      accumulated
    } else {
      iter(rest(arr), push(accumulated, f(first(arr))))
    }
  };
  iter(arr, [])
};
let reduce = fn(arr, initial, f) {
  let iter = fn(arr, result) {
    if (len(arr) == 0) {
      result
    } else {
      iter(rest(arr), f(result, first(arr)))
    }
  };
  iter(arr, initial)
};
let sum = fn(arr) {
  reduce(arr, 0, fn(initial, el) { initial + el })
};
let filter = fn(arr, predicate) {
  let iter = fn(arr, filtered) {
    if (len(arr) == 0) {
      filtered
    } else {
      let firstElement = first(arr);
      let restElements = rest(arr);
      if (predicate(firstElement)) {
        iter(restElements, push(filtered, firstElement))
      } else {
        iter(restElements, filtered)
      }
    }
  };
  iter(arr, [])
};

";

pub fn instaniate_std_lib(env: &Env) {
    let lexer = Lexer::new(STD_LIB.to_string());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();
    program
        .eval(env)
        .expect("std lib shouldnt fail to evaluate");
}

fn len(args: Vec<Rc<Object>>) -> Result<Rc<Object>, BuiltInFunctionError> {
    if args.len() != 1 {
        return Err(BuiltInFunctionError {
            error: BIFInnerError::WrongNumberOfArguments {
                expected: 1,
                got: args.len(),
            },
            function: BuiltInFunction {
                function: len,
                type_signature: type_signatures::LEN,
            },
            message: None,
        });
    }

    match &*args[0] {
        Object::String(s) => Ok(Rc::new(Object::Integer(s.len() as isize))),
        Object::Array(a) => Ok(Rc::new(Object::Integer(a.len() as isize))),
        _ => Err(BuiltInFunctionError {
            error: BIFInnerError::WrongArgumentType {
                expected: ObjectExpectation::Many(vec![
                    Rc::new(Object::String("".to_string())),
                    Rc::new(Object::Array(vec![])),
                ]),
                got: args[0].clone(),
            },
            function: BuiltInFunction {
                function: len,
                type_signature: type_signatures::LEN,
            },
            message: None,
        }),
    }
}

fn first(args: Vec<Rc<Object>>) -> Result<Rc<Object>, BuiltInFunctionError> {
    if args.len() != 1 {
        return Err(BuiltInFunctionError {
            error: BIFInnerError::WrongNumberOfArguments {
                expected: 1,
                got: args.len(),
            },
            function: BuiltInFunction {
                function: first,
                type_signature: type_signatures::FIRST,
            },
            message: None,
        });
    }

    match &*args[0] {
        Object::Array(a) => match a.first() {
            Some(o) => Ok(Rc::clone(o)),
            None => Ok(Rc::new(Object::None)),
        },
        _ => Err(BuiltInFunctionError {
            error: BIFInnerError::WrongArgumentType {
                expected: ObjectExpectation::One(Rc::new(Object::Array(vec![]))),
                got: args[0].clone(),
            },
            function: BuiltInFunction {
                function: first,
                type_signature: type_signatures::FIRST,
            },
            message: None,
        }),
    }
}

fn last(args: Vec<Rc<Object>>) -> Result<Rc<Object>, BuiltInFunctionError> {
    if args.len() != 1 {
        return Err(BuiltInFunctionError {
            error: BIFInnerError::WrongNumberOfArguments {
                expected: 1,
                got: args.len(),
            },
            function: BuiltInFunction {
                function: last,
                type_signature: type_signatures::FIRST,
            },
            message: None,
        });
    }

    match &*args[0] {
        Object::Array(a) => match a.last() {
            Some(o) => Ok(Rc::clone(o)),
            None => Ok(Rc::new(Object::None)),
        },
        _ => Err(BuiltInFunctionError {
            error: BIFInnerError::WrongArgumentType {
                expected: ObjectExpectation::One(Rc::new(Object::Array(vec![]))),
                got: args[0].clone(),
            },
            function: BuiltInFunction {
                function: last,
                type_signature: type_signatures::FIRST,
            },
            message: None,
        }),
    }
}

fn rest(args: Vec<Rc<Object>>) -> Result<Rc<Object>, BuiltInFunctionError> {
    if args.len() != 1 {
        return Err(BuiltInFunctionError {
            error: BIFInnerError::WrongNumberOfArguments {
                expected: 1,
                got: args.len(),
            },
            function: BuiltInFunction {
                function: rest,
                type_signature: type_signatures::FIRST,
            },
            message: None,
        });
    }

    match &*args[0] {
        Object::Array(a) => {
            if a.is_empty() {
                return Ok(Rc::new(Object::None));
            }
            let rest = &a[1..];
            Ok(Rc::new(Object::Array(rest.to_vec())))
        }
        _ => Err(BuiltInFunctionError {
            error: BIFInnerError::WrongArgumentType {
                expected: ObjectExpectation::One(Rc::new(Object::Array(vec![]))),
                got: args[0].clone(),
            },
            function: BuiltInFunction {
                function: rest,
                type_signature: type_signatures::FIRST,
            },
            message: None,
        }),
    }
}

fn push(args: Vec<Rc<Object>>) -> Result<Rc<Object>, BuiltInFunctionError> {
    if args.len() != 2 {
        return Err(BuiltInFunctionError {
            error: BIFInnerError::WrongNumberOfArguments {
                expected: 2,
                got: args.len(),
            },
            function: BuiltInFunction {
                function: push,
                type_signature: type_signatures::PUSH,
            },
            message: None,
        });
    }

    match &*args[0] {
        Object::Array(a) => {
            let mut new_array = a.clone();
            new_array.push(Rc::clone(&args[1]));
            Ok(Rc::new(Object::Array(new_array)))
        }
        _ => Err(BuiltInFunctionError {
            error: BIFInnerError::WrongArgumentType {
                expected: ObjectExpectation::One(Rc::new(Object::Array(vec![]))),
                got: Rc::clone(&args[0]),
            },
            function: BuiltInFunction {
                function: push,
                type_signature: type_signatures::PUSH,
            },
            message: None,
        }),
    }
}

#[cfg(test)]
mod tests {

    use crate::evaluator::{test_vs_expectation, EvalError};

    use super::*;

    #[test]
    fn test_len() {
        let pairs = vec![
            (r#"len("")"#, Ok(Object::Integer(0))),
            (r#"len("four")"#, Ok(Object::Integer(4))),
            (r#"len("hello world")"#, Ok(Object::Integer(11))),
            (
                r#"len("one", "two")"#,
                Err(EvalError::BuiltInFunction(BuiltInFunctionError {
                    error: BIFInnerError::WrongNumberOfArguments {
                        expected: 1,
                        got: 2,
                    },
                    function: BuiltInFunction {
                        function: len,
                        type_signature: type_signatures::LEN,
                    },
                    message: None,
                })),
            ),
            (
                "len(5)",
                Err(EvalError::BuiltInFunction(BuiltInFunctionError {
                    error: BIFInnerError::WrongArgumentType {
                        expected: ObjectExpectation::Many(vec![
                            Rc::new(Object::String("".to_string())),
                            Rc::new(Object::Array(vec![])),
                        ]),
                        got: Rc::new(Object::Integer(5)),
                    },
                    function: BuiltInFunction {
                        function: len,
                        type_signature: type_signatures::LEN,
                    },
                    message: None,
                })),
            ),
            ("len([])", Ok(Object::Integer(0))),
            ("len([1, 2, 3])", Ok(Object::Integer(3))),
        ];

        test_vs_expectation(pairs);
    }

    #[test]
    fn test_first() {
        let pairs = vec![
            ("first([1, 2, 3])", Ok(Object::Integer(1))),
            ("first([])", Ok(Object::None)),
            (
                r#"first("test")"#,
                Err(EvalError::BuiltInFunction(BuiltInFunctionError {
                    error: BIFInnerError::WrongArgumentType {
                        expected: ObjectExpectation::One(Rc::new(Object::Array(vec![]))),
                        got: Rc::new(Object::String("test".to_string())),
                    },
                    function: BuiltInFunction {
                        function: first,
                        type_signature: type_signatures::FIRST,
                    },
                    message: None,
                })),
            ),
        ];

        test_vs_expectation(pairs);
    }

    #[test]
    fn test_last() {
        let pairs = vec![
            ("last([1, 2, 3])", Ok(Object::Integer(3))),
            ("last([])", Ok(Object::None)),
            (
                r#"last("test")"#,
                Err(EvalError::BuiltInFunction(BuiltInFunctionError {
                    error: BIFInnerError::WrongArgumentType {
                        expected: ObjectExpectation::One(Rc::new(Object::Array(vec![]))),
                        got: Rc::new(Object::String("test".to_string())),
                    },
                    function: BuiltInFunction {
                        function: last,
                        type_signature: type_signatures::FIRST,
                    },
                    message: None,
                })),
            ),
        ];

        test_vs_expectation(pairs);
    }

    #[test]
    fn test_rest() {
        let pairs = vec![
            (
                "rest([1, 2, 3])",
                Ok(Object::Array(vec![
                    Rc::new(Object::Integer(2)),
                    Rc::new(Object::Integer(3)),
                ])),
            ),
            ("rest([])", Ok(Object::None)),
            (
                r#"rest("test")"#,
                Err(EvalError::BuiltInFunction(BuiltInFunctionError {
                    error: BIFInnerError::WrongArgumentType {
                        expected: ObjectExpectation::One(Rc::new(Object::Array(vec![]))),
                        got: Rc::new(Object::String("test".to_string())),
                    },
                    function: BuiltInFunction {
                        function: rest,
                        type_signature: type_signatures::FIRST,
                    },
                    message: None,
                })),
            ),
        ];

        test_vs_expectation(pairs);
    }

    #[test]
    fn test_push() {
        let pairs = vec![
            (
                "push([1, 2], 3)",
                Ok(Object::Array(vec![
                    Rc::new(Object::Integer(1)),
                    Rc::new(Object::Integer(2)),
                    Rc::new(Object::Integer(3)),
                ])),
            ),
            (
                r#"push("test", 1)"#,
                Err(EvalError::BuiltInFunction(BuiltInFunctionError {
                    error: BIFInnerError::WrongArgumentType {
                        expected: ObjectExpectation::One(Rc::new(Object::Array(vec![]))),
                        got: Rc::new(Object::String("test".to_string())),
                    },
                    function: BuiltInFunction {
                        function: push,
                        type_signature: type_signatures::PUSH,
                    },
                    message: None,
                })),
            ),
        ];

        test_vs_expectation(pairs);
    }

    mod std_lib {

        use super::*;

        #[test]

        fn test_map() {
            let pairs = vec![(
                "map([1, 2, 3], fn(x) { x * 2 })",
                Ok(Object::Array(vec![
                    Rc::new(Object::Integer(2)),
                    Rc::new(Object::Integer(4)),
                    Rc::new(Object::Integer(6)),
                ])),
            )];

            test_vs_expectation(pairs);
        }

        #[test]
        fn test_filter() {
            let pairs = vec![(
                "filter([1, 2, 3, 4], fn(x) { x % 2 == 0 })",
                Ok(Object::Array(vec![
                    Rc::new(Object::Integer(2)),
                    Rc::new(Object::Integer(4)),
                ])),
            )];

            test_vs_expectation(pairs);
        }

        #[test]
        fn test_reduce() {
            let pairs = vec![(
                "reduce([1, 2, 3], 0, fn(acc, x) { acc + x })",
                Ok(Object::Integer(6)),
            )];

            test_vs_expectation(pairs);
        }

        #[test]
        fn test_sum() {
            let pairs = vec![
                ("sum([1, 2, 3])", Ok(Object::Integer(6))),
                ("sum([])", Ok(Object::Integer(0))),
            ];

            test_vs_expectation(pairs);
        }
    }
}
