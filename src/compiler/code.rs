use byteorder::{BigEndian, WriteBytesExt};

pub struct Instructions {
    pub data: Vec<u8>,
}

#[repr(u8)]
pub enum OpCode {
    Constant,
}

impl From<u8> for OpCode {
    fn from(item: u8) -> Self {
        match item {
            0 => OpCode::Constant,
            _ => panic!("byte is not a valid opcode"),
        }
    }
}

impl Into<u8> for OpCode {
    fn into(self) -> u8 {
        match self {
            OpCode::Constant => 0,
        }
    }
}

pub struct Definition {
    pub name: &'static str,
    pub operand_widths: Vec<usize>,
}

impl OpCode {
    pub fn get_definition(&self) -> Definition {
        match self {
            OpCode::Constant => Definition {
                name: "Constant",
                operand_widths: vec![2],
            },
        }
    }
}

pub fn make(op: OpCode, operands: &[usize]) -> Instructions {
    let definition = op.get_definition();
    let widths = definition.operand_widths;
    let instruction_len = widths.iter().sum::<usize>() + 1;
    let mut instructions = Vec::with_capacity(instruction_len);

    instructions.push(op.into());

    for (operand, width) in operands.iter().zip(widths) {
        match width {
            2 => {
                if *operand <= u16::max_value() as usize {
                    instructions
                        .write_u16::<BigEndian>(*operand as u16)
                        .unwrap();
                } else {
                    panic!("operand value {} exceeds u16 size", *operand);
                }
            }
            1 => {
                if *operand <= u8::max_value() as usize {
                    instructions.write_u8(*operand as u8).unwrap();
                } else {
                    panic!("operand value {} exceeds u8 size", *operand);
                }
            }
            _ => {
                panic!("unsupported operand width {}", width);
            }
        }
    }

    Instructions { data: instructions }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_make() {
        let input = (OpCode::Constant, &[65534]);
        let expected = vec![OpCode::Constant.into(), 255, 254];

        let actual = make(input.0, input.1).data;

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_to_string() {
        let instructions = vec![
            make(OpCode::Constant, &[1]),
            make(OpCode::Constant, &[2]),
            make(OpCode::Constant, &[65535]),
        ];

        let expected = "0000 OpConstant 1\n0003 OpConstant 2\n0006 OpConstant 65535\n";

        let actual = instructions_to_string(instructions);

        assert_eq!(actual, expected);
    }
}
