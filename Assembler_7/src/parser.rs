use std::fs;
use std::io::{Error, ErrorKind};


pub fn parse(input_args: &[String]) -> Result<Vec<String>, Error> {
    let file_contents = open_file(input_args)?;
    let parsed_contents = parse_file(&file_contents);

    match parsed_contents.is_empty() {
        true => return Err(Error::new(ErrorKind::InvalidData, "empty file")),
        false => return Ok(parsed_contents),
    }
}

fn open_file(input_args: &[String]) -> Result<String, Error> {
    let file_name: String;
    match input_args.len() {
        2 => file_name = input_args[1].clone(),
        _ => return Err(Error::new(ErrorKind::Other, "Invalid number of arguments")),
    };

    if &file_name[&file_name.len() - 2..] != "vm" {
        return Err(Error::new(ErrorKind::Other, "Invalid file type"));
    }
    let file = fs::read_to_string(file_name)?;
    Ok(file)
}

fn parse_line(line: &str) -> &str {
    if line == "" || line.chars().nth(0).unwrap() == '/' {
        return "";
    } else {
        return line.split("//").nth(0).unwrap().trim();
    }
}

fn parse_file(file_contents: &String) -> Vec<String> {
    let mut parsed_content: Vec<String> = vec![];

    for line in file_contents.lines().map(|x| x.trim()) {
        let parsed_line = parse_line(line);
        if parsed_line == "" {
            continue;
        } else {
            parsed_content.push(parsed_line.to_owned());
        }
    }
    parsed_content
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
    GeneralPurpose, // for code writing
}

#[derive(Debug, PartialEq)]
pub struct MemoryLocation {
    pub segment: MemorySegment,
    pub index: u16,
}

impl MemoryLocation {
    fn assign(seg: &str) -> Result<MemorySegment, Error> {
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
    C_ARITHMETIC(ArithmeticCommands),
    C_PUSH(MemoryLocation),
    C_POP(MemoryLocation),
    C_LABEL,
    C_GOTO,
    C_IF_C_FUNCTION,
    C_RETURN,
    C_CALLS,
}

pub fn parse_current_command(command: &str) -> Result<Instruction, Error> {
    let command_split = command.split(" ").collect::<Vec<&str>>();

    match command_split.len() {
        3 => {
            let to_seg = MemoryLocation::assign(&command_split[1])?;
            let to_index = command_split[2].parse::<u16>().unwrap();

            match command_split[0] {
                "push" => Ok(Instruction::C_PUSH(MemoryLocation {
                    segment: to_seg,
                    index: to_index,
                })),
                "pop" => Ok(Instruction::C_POP(MemoryLocation {
                    segment: to_seg,
                    index: to_index,
                })),
                _ => Err(Error::new(
                    ErrorKind::InvalidInput,
                    format!("Invalid command found: {:?}", command_split),
                )),
            }
        }
        1 => match command_split[0] {
            "add" => Ok(Instruction::C_ARITHMETIC(ArithmeticCommands::Add)),
            "sub" => Ok(Instruction::C_ARITHMETIC(ArithmeticCommands::Sub)),
            "and" => Ok(Instruction::C_ARITHMETIC(ArithmeticCommands::And)),
            "or" => Ok(Instruction::C_ARITHMETIC(ArithmeticCommands::Or)),
            "neg" => Ok(Instruction::C_ARITHMETIC(ArithmeticCommands::Neg)),
            "not" => Ok(Instruction::C_ARITHMETIC(ArithmeticCommands::Not)),
            "eq" => Ok(Instruction::C_ARITHMETIC(ArithmeticCommands::Eq)),
            "gt" => Ok(Instruction::C_ARITHMETIC(ArithmeticCommands::Gt)),
            "lt" => Ok(Instruction::C_ARITHMETIC(ArithmeticCommands::Lt)),
            _ => Err(Error::new(
                ErrorKind::InvalidInput,
                format!("Invalid command found: {:?}", command_split),
            )),
        },
        _ => Err(Error::new(
            ErrorKind::InvalidInput,
            format!("Invalid command found: {:?}", command_split),
        )),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    // Commands test
    #[test]
    fn push_command() {
        assert_eq!(
            parse_current_command("push constant 10").unwrap(),
            Instruction::C_PUSH(MemoryLocation {
                segment: MemorySegment::Constant,
                index: 10
            })
        )
    }

    #[test]
    fn pop_command() {
        assert_eq!(
            parse_current_command("pop static 1").unwrap(),
            Instruction::C_POP(MemoryLocation {
                segment: MemorySegment::Static,
                index: 1
            })
        );
    }

    #[test]
    fn test_parse_others() {
        assert_eq!(
            parse_current_command("add").unwrap(),
            Instruction::C_ARITHMETIC(ArithmeticCommands::Add)
        );
        assert_eq!(
            parse_current_command("sub").unwrap(),
            Instruction::C_ARITHMETIC(ArithmeticCommands::Sub)
        );
        /*
        assert_eq!(parse_current_command("and").unwrap(), Instruction::C_ARITHMETIC(ArithmeticCommands::And));
        assert_eq!(parse_current_command("or").unwrap(), Instruction::C_ARITHMETIC(ArithmeticCommands::Or));

        assert_eq!(parse_current_command("neg").unwrap(), Instruction::C_ARITHMETIC(ArithmeticCommands::Neg));
        assert_eq!(parse_current_command("not").unwrap(), Instruction::C_ARITHMETIC(ArithmeticCommands::Not));

        assert_eq!(parse_current_command("eq").unwrap(), Instruction::C_ARITHMETIC(ArithmeticCommands::Eq));
        assert_eq!(parse_current_command("gt").unwrap(), Instruction::C_ARITHMETIC(ArithmeticCommands::Gt));
        assert_eq!(parse_current_command("lt").unwrap(), Instruction::C_ARITHMETIC(ArithmeticCommands::Lt));
        */
    }

    // File tests
    #[test]
    fn invalid_args() {
        let expected = Err(ErrorKind::Other);

        let too_many = &[
            "too".to_owned(),
            "BasicTest.vm".to_owned(),
            "args".to_owned(),
        ];
        let result = open_file(too_many).map_err(|e| e.kind());
        assert_eq!(result, expected);

        let too_few = &["toofew".to_owned()];
        let result = open_file(too_few).map_err(|e| e.kind());
        assert_eq!(result, expected);

        let none = &[];
        let result = open_file(none).map_err(|e| e.kind());
        assert_eq!(result, expected);
    }

    #[test]
    #[should_panic]
    fn invalid_file_path() {
        let test_args = &["_".to_owned(), "test123.vm".to_owned()];
        match open_file(test_args) {
            Ok(res) => println!("{:?}", res),
            Err(_) => panic!("invalid file"),
        };
    }

    #[test]
    fn wrong_file_type() {
        let test_args = &["_".to_owned(), "fileType.txt".to_owned()];
        assert!(open_file(test_args).is_err());
        let test_args = &["_".to_owned(), "BasicTest.vm".to_owned()];
        assert!(open_file(test_args).is_ok());
    }

    #[test]
    fn valid_open() {
        let test_args = &["_".to_owned(), "BasicTest.vm".to_owned()];
        assert!(open_file(test_args).is_ok())
    }
}
