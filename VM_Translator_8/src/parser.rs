use std::fs;
use std::io::{Error, ErrorKind};

pub fn relevant_files(file_or_directory_name: &str) -> Vec<String> {
    let mut file_names: Vec<String> = vec![];
    if let Ok(paths) = fs::read_dir(file_or_directory_name) {
        for path in paths {
            let file_path = path.unwrap().path().into_os_string().into_string().unwrap();
            if file_path.contains(".vm") {
                file_names.push(file_path);
            }
        }
    } else {
        file_names.push(file_or_directory_name.to_owned());
    }

    if file_names.is_empty() {
        panic!("No vm files found"); // unrecoverable error
    } 

    file_names
}

pub fn parse_file(file_path: &String) -> Result<Vec<String>, Error> {
    let file_contents = fs::read_to_string(&file_path)?;

    let parsed_contents = parse_file_contents(&file_contents);

    match parsed_contents.is_empty() {
        true => Err(Error::new(ErrorKind::InvalidData, "Empty file used")),
        false => Ok(parsed_contents),
    }
}

fn parse_file_contents(file_contents: &String) -> Vec<String> {
    let mut parsed_content: Vec<String> = vec![];

    for line in file_contents.lines().map(|x| x.trim()) {
        let parsed_line = parse_line(line);
        if let Some(line_to_push) = parsed_line {
            parsed_content.push(line_to_push.to_owned())
        };
    }
    parsed_content
}

fn parse_line(line: &str) -> Option<&str> {
    if line == "" || line.chars().nth(0).unwrap() == '/' {
        return None;
    } else if line.contains("//") {
        // returns line
        return Some(line.split("//").nth(0).unwrap().trim());
    } else {
        return Some(line);
    }
}

#[derive(Debug, PartialEq, Hash, Eq)]
pub enum MemorySegment {
    SP,
    Local,
    Argument,
    This,
    That,
    Constant,
    Pointer,
    Temp,
    Static,
    GeneralPurpose, // Spare registers (R13-15)
}

#[derive(Debug, PartialEq)]
pub struct StackBaseMemory {
    pub segment: MemorySegment,
    pub index: u16,
}

impl StackBaseMemory {
    fn match_segment(seg: &str) -> Result<MemorySegment, Error> {
        match seg {
            "SP" => Ok(MemorySegment::SP),
            "local" => Ok(MemorySegment::Local),
            "argument" => Ok(MemorySegment::Argument),
            "this" => Ok(MemorySegment::This),
            "that" => Ok(MemorySegment::That),
            "constant" => Ok(MemorySegment::Constant),
            "pointer" => Ok(MemorySegment::Pointer),
            "temp" => Ok(MemorySegment::Temp),
            "static" => Ok(MemorySegment::Static),
            _ => Err(Error::new(
                ErrorKind::InvalidInput,
                format!("Invalid memory segment found: {:?}", seg),
            )),
        }
    }
    pub fn segment_base_index(seg: &MemorySegment) -> u16 {
        match seg {
            MemorySegment::SP => 0,
            MemorySegment::Local => 1,
            MemorySegment::Argument => 2,
            MemorySegment::This => 3,
            MemorySegment::That => 4,
            MemorySegment::Constant => {
                panic!("The constant segment shouldn't be used in the segment_base_index function")
            }
            MemorySegment::Pointer => 3,
            MemorySegment::Temp => 5,
            MemorySegment::GeneralPurpose => 13,
            MemorySegment::Static => 16,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ArithmeticCommands {
    Add,
    Sub,
    And,
    Or,
    Neg,
    Not,
    Eq,
    Gt,
    Lt,
}

#[derive(Debug, PartialEq)]
pub enum Instruction {
    CArithmetic(ArithmeticCommands),
    CPush(StackBaseMemory),
    CPop(StackBaseMemory),
    CLabel(String),
    CGoto(String),
    CIfGoto(String),
    CFunction(String, u16),
    CReturn,
    CCalls(String, u16),
}

pub fn parse_current_command(command: &str) -> Result<Instruction, Error> {
    let command_split = command.split_whitespace().collect::<Vec<&str>>();

    let command_str = command_split[0];
    match command_split.len() {
        1 => match command_str {
            "add" => Ok(Instruction::CArithmetic(ArithmeticCommands::Add)),
            "sub" => Ok(Instruction::CArithmetic(ArithmeticCommands::Sub)),
            "and" => Ok(Instruction::CArithmetic(ArithmeticCommands::And)),
            "or" => Ok(Instruction::CArithmetic(ArithmeticCommands::Or)),
            "neg" => Ok(Instruction::CArithmetic(ArithmeticCommands::Neg)),
            "not" => Ok(Instruction::CArithmetic(ArithmeticCommands::Not)),
            "eq" => Ok(Instruction::CArithmetic(ArithmeticCommands::Eq)),
            "gt" => Ok(Instruction::CArithmetic(ArithmeticCommands::Gt)),
            "lt" => Ok(Instruction::CArithmetic(ArithmeticCommands::Lt)),
            "return" => Ok(Instruction::CReturn),
            _ => Err(parser_error_message(command_split)),
        },
        2 => match command_str {
            "label" => Ok(Instruction::CLabel(command_split[1].to_owned())),
            "goto" => Ok(Instruction::CGoto(command_split[1].to_owned())),
            "if-goto" => Ok(Instruction::CIfGoto(command_split[1].to_owned())),

            _ => Err(parser_error_message(command_split)),
        },
        3 => {
            let index = command_split[2].parse::<u16>().unwrap();
            match command_str {
                "push" => {
                    let segment = StackBaseMemory::match_segment(&command_split[1]).unwrap();
                    return Ok(Instruction::CPush(StackBaseMemory { segment, index }));
                }
                "pop" => {
                    let segment = StackBaseMemory::match_segment(&command_split[1]).unwrap();
                    return Ok(Instruction::CPop(StackBaseMemory { segment, index }));
                }
                "function" => {
                    let name = command_split[1].to_owned();
                    return Ok(Instruction::CFunction(name, index));
                }
                "call" => {
                    let name = command_split[1].to_owned();
                    return Ok(Instruction::CCalls(name, index));
                }
                _ => Err(parser_error_message(command_split)),
            }
        }

        _ => Err(parser_error_message(command_split)),
    }
}

fn parser_error_message(curr_command: Vec<&str>) -> Error {
    let error_message = "Error: Invalid command found at parse_curr_command: ";
    Error::new(
        ErrorKind::InvalidInput,
        format!("{}: {:?}", error_message, curr_command),
    )
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn parse_line_test() {
        assert_eq!(parse_line("hi"), Some("hi"))
    }
    // Commands test
    #[test]
    fn push_command() {
        assert_eq!(
            parse_current_command("push constant 10").unwrap(),
            Instruction::CPush(StackBaseMemory {
                segment: MemorySegment::Constant,
                index: 10
            })
        )
    }

    #[test]
    fn pop_command() {
        assert_eq!(
            parse_current_command("pop static 1").unwrap(),
            Instruction::CPop(StackBaseMemory {
                segment: MemorySegment::Static,
                index: 1
            })
        );
    }

    #[test]
    fn test_parse_others() {
        assert_eq!(
            parse_current_command("add").unwrap(),
            Instruction::CArithmetic(ArithmeticCommands::Add)
        );
        assert_eq!(
            parse_current_command("sub").unwrap(),
            Instruction::CArithmetic(ArithmeticCommands::Sub)
        );
        /*
        assert_eq!(parse_current_command("and").unwrap(), Instruction::CArithmetic(ArithmeticCommands::And));
        assert_eq!(parse_current_command("or").unwrap(), Instruction::CArithmetic(ArithmeticCommands::Or));

        assert_eq!(parse_current_command("neg").unwrap(), Instruction::CArithmetic(ArithmeticCommands::Neg));
        assert_eq!(parse_current_command("not").unwrap(), Instruction::CArithmetic(ArithmeticCommands::Not));

        assert_eq!(parse_current_command("eq").unwrap(), Instruction::CArithmetic(ArithmeticCommands::Eq));
        assert_eq!(parse_current_command("gt").unwrap(), Instruction::CArithmetic(ArithmeticCommands::Gt));
        assert_eq!(parse_current_command("lt").unwrap(), Instruction::CArithmetic(ArithmeticCommands::Lt));
        */
    }

    // File tests
    #[test]
    #[should_panic]
    fn invalid_file_path() {
        let test_args = &"test123.vm".to_owned();
        match parse_file(test_args) {
            Ok(res) => println!("{:?}", res),
            Err(_) => panic!("invalid file"),
        };
    }

    #[test]
    fn wrong_file_type() {
        let test_args = &"fileType.txt".to_owned();
        assert!(parse_file(test_args).is_err());
        let test_args = &"BasicTest.vm".to_owned();
        assert!(parse_file(test_args).is_ok());
    }
}
