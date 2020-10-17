use super::parser::{ArithmeticCommands, Instruction, MemoryLocation, MemorySegment};
use std::collections::HashMap;

use std::env; //for args

// Static hashmap for RAM indexes

lazy_static! {
    //static ref static_file_name: &'static str = &env::args().nth(1).unwrap();

    static ref RAM_SEG_NAME: HashMap<MemorySegment, &'static str> = {
        let mut map = HashMap::new();
        map.insert(MemorySegment::SP, "SP");
        map.insert(MemorySegment::Local, "LCL");
        map.insert(MemorySegment::Argument, "ARG");
        map.insert(MemorySegment::This, "THIS");
        map.insert(MemorySegment::That, "THAT");
        map.insert(MemorySegment::Temp, "TEMP");
        // map.insert(MemorySegment::Pointer, "POINTER");
        //map.insert(MemorySegment::GeneralPurpose, 13)
        //map.insert(MemorySegment::Static, &file_name);

        map
    };

    static ref RAM_SEG_BASE_INDEX: HashMap<MemorySegment, u16> = {
        let mut map = HashMap::new();
        map.insert(MemorySegment::SP, 0); //SP M = 255 < x <= 2047
        map.insert(MemorySegment::Local, 1);
        map.insert(MemorySegment::Argument, 2);
        map.insert(MemorySegment::Pointer, 3); // index -> 0/1
        map.insert(MemorySegment::This, 3);
        map.insert(MemorySegment::That, 4);
        map.insert(MemorySegment::Temp, 5);
        map.insert(MemorySegment::GeneralPurpose, 13);
        map.insert(MemorySegment::Static, 16);

        map
    };
}

pub fn write_command(command: &Instruction, file_name: &str) -> Vec<String> {
    match command {
        Instruction::C_PUSH(ref push_command) => write_push(&push_command, file_name),
        Instruction::C_POP(ref pop_command) => write_pop(&pop_command, file_name),
        Instruction::C_ARITHMETIC(ref arithmetic_command) => write_arithmetic(&arithmetic_command),
        Instruction::C_LABEL => vec![],
        Instruction::C_GOTO => vec![],
        Instruction::C_IF_C_FUNCTION => vec![],
        Instruction::C_RETURN => vec![],
        Instruction::C_CALLS => vec![],
    }
}

fn write_arithmetic(command: &ArithmeticCommands) -> Vec<String> {
    
    let mut operator = match command {
        ArithmeticCommands::Add => "+",
        ArithmeticCommands::Sub => "-",
        ArithmeticCommands::And => "&",
        ArithmeticCommands::Or => "|",
        
        ArithmeticCommands::Neg => "-",
        ArithmeticCommands::Not => "!",

        ArithmeticCommands::Eq => "JEQ",
        ArithmeticCommands::Gt => "JGT",
        ArithmeticCommands::Lt => "JLT",
    };

    let mut command_vector: Vec<String> = vec![];

    let comparison_operator = if operator == "JEQ" || operator == "JGT" || operator == "JLT" {true} else {false};
    
    
    if !comparison_operator {
        let binary_operation = if operator != "-" || operator != "!" {true} else {false};
        let operand = if binary_operation {"D"} else {""};

        if binary_operation {
            command_vector.extend(register_d("pop"));
        } 

        let next_pop = vec![
                format!("@{}", RAM_SEG_NAME[&MemorySegment::SP]),
                "M=M-1".to_owned(), // decrement to top of stack
                "A=M".to_owned(), //we go to address pointed to by SP
                format!("D={}{}M", operand, operator)
        ];
        command_vector.extend(next_pop);
        command_vector

    } else{ //comparison operator
        command_vector.extend(register_d("pop"));
        let next_pop = vec![
            format!("@{}", RAM_SEG_NAME[&MemorySegment::SP]),
            "M=M-1".to_owned(),
            "A=M".to_owned(),
            "D=D-M".to_owned(), // compare two numbers
            format!("D;{}", operator)
        ];
        command_vector.extend(next_pop);

        command_vector
    }
}
fn write_push(command: &MemoryLocation, file_name: &str) -> Vec<String> {
    let mut command_vector: Vec<String> = register_a(command, file_name);

    if command.segment == MemorySegment::Constant {
        command_vector.push("D=A".to_owned());
    } else {
        command_vector.push("D=M".to_owned());
    }
    command_vector.extend(register_d("push"));
    command_vector
}

fn write_pop(command: &MemoryLocation, file_name: &str) -> Vec<String> {
    // store in temp
    let store_as_temp: Vec<String> = vec![
        "D=A".to_owned(),
        format!("@R{}", RAM_SEG_BASE_INDEX[&MemorySegment::GeneralPurpose]), // Reg 13-15 are free to use
        "A=M".to_owned(),
        "M=D".to_owned(),
    ];
    let save_to_memory = vec![
        format!("@R{}", RAM_SEG_BASE_INDEX[&MemorySegment::GeneralPurpose]), // Temp ram to A register
        "A=M".to_string(), // dereference
        "M=D".to_string(), // save to memory
    ];

    let mut command_vector: Vec<String> = register_a(command, file_name);
    command_vector.extend(store_as_temp);
    command_vector.extend(register_d("pop"));
    command_vector.extend(save_to_memory);
    command_vector
}

fn register_a(command: &MemoryLocation, file_name: &str) -> Vec<String> {
    let ram_seg_name = match command.segment {
        MemorySegment::Constant => return vec![format!("@{}", command.index)],

        MemorySegment::Pointer | MemorySegment::Temp => {
            return vec![format!("@{}", RAM_SEG_BASE_INDEX[&command.segment] + command.index)]
        }
        MemorySegment::Static => return vec![format!("@{}.{}", file_name, command.index)],
        // else, ram_seg_name -> i.e. command.index means a value to be used
        MemorySegment::Local
        | MemorySegment::Argument
        | MemorySegment::This
        | MemorySegment::That => RAM_SEG_NAME[&command.segment],

        _ => "need to figure out a better way later!",
    };
    vec![
        format!("@{}", command.index),
        "D=A".to_owned(),
        format!("@{}", ram_seg_name),
        "A=M+D".to_owned(),
    ]
}

fn register_d(push_or_pop: &str) -> Vec<String> {
    if push_or_pop == "push" {
        return vec![
            // Place value into the next available address on stack (stack pointer)
            format!("@{}", RAM_SEG_NAME[&MemorySegment::SP]),
            "A=M".to_owned(),
            "M=D".to_owned(),
            // Increment SP
            format!("@{}", RAM_SEG_NAME[&MemorySegment::SP]),
            "M=M+1".to_owned(),
        ];
    } else if push_or_pop == "pop" {
        return vec![
            // Decrement SP
            format!("@{}", RAM_SEG_NAME[&MemorySegment::SP]),
            "M=M-1".to_owned(),
            //
            "A=M".to_owned(), //we go to address pointed to by SP
            "D=M".to_owned(), // Store value in D
        ];
    } else {
        panic!("Invalid input on register d function - this shouldn't happen anyways as private");
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn push_command() {
        let push_const = MemoryLocation {segment: MemorySegment::Constant, index: 10 };
        let file_name = "test";
        assert_eq!(
            write_push(&push_const, &file_name),
            vec!["@10", "D=A", "@SP", "A=M", "M=D", "@SP", "M=M+1"]
        );

        let push_non_const = MemoryLocation {segment: MemorySegment::Static, index: 20};
        assert_eq!(
            write_push(&push_non_const, &file_name),
            vec!["@test.20", "D=M", "@SP", "A=M", "M=D", "@SP", "M=M+1"]
        )
    }

    #[test]
    fn pop_command(){
        let pop_const = MemoryLocation {segment: MemorySegment::Constant, index: 10 };
        let file_name = "test";
        assert_eq!(
            write_pop(&pop_const, &file_name),
            vec!["@10", "D=A", "@SP", "A=M", "M=D", "@SP", "M=M+1"]
        );
    }

}

/*
Code writer
Writes assembly code that implements the parsed command
-   write the command as a command for easier debugging later

API:
Constructor -> output file/stream -> _ -> opens the output file to prepare to write into
WriteArithmetic -> command (string) -> _ -> write into output the assembly code that
                                            implements the arithmetic command
WritePushPop -> command (C_PUSH / C_POP), segment (string), indx(int) -> _
                        -> writes to output the assembly code of either C_PUSH / C_POP command
Close -> _ -> _ -> closes the output file
    more routines will be added to this module in proj 8.
*/
