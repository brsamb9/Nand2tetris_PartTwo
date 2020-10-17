use super::parser::{ArithmeticCommands, Instruction, MemorySegment, StackBaseMemory};

pub fn write_init() -> Vec<String> {
    let mut bootstrap: Vec<String> = vec![
        // Initalise SP
        "@256".to_owned(),
        "D=A".to_owned(),
        "@SP".to_owned(),
        "M=D".to_owned(),
    ];
    // Initalise Sys.init (OS-level command)
    bootstrap.extend(write_calls("Sys.init", &(0 as u16)));

    bootstrap
}

pub fn write_command(command: &Instruction, file_name: &str) -> Vec<String> {
    // Top level of code-writer: After parser
    // filters towards the correct procedure for each respective command
    match command {
        Instruction::CPush(ref push_command) => write_push(&push_command, file_name),
        Instruction::CPop(ref pop_command) => write_pop(&pop_command, file_name),
        Instruction::CArithmetic(ref arithmetic_or_logic_command) => {
            write_arithmetic_or_logic(&arithmetic_or_logic_command)
        }
        Instruction::CLabel(ref label) => write_label(label),
        Instruction::CGoto(ref label) => write_goto(label),
        Instruction::CIfGoto(ref label) => write_if_goto(label),
        Instruction::CFunction(ref name, n_vars) => write_function(name, n_vars),
        Instruction::CReturn => write_return(),
        Instruction::CCalls(ref name, n_args) => write_calls(name, n_args),
    }
}

fn write_arithmetic_or_logic(command: &ArithmeticCommands) -> Vec<String> {
    let operator = match command {
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

    let comparison_operator = if operator == "JEQ" || operator == "JGT" || operator == "JLT" {
        true
    } else {
        false
    };

    if comparison_operator {
        write_comparison(&operator)
    } else {
        write_arithmetic(&operator)
    }
}

fn write_arithmetic(operator: &str) -> Vec<String> {
    let binary_operation = if operator != "-" && operator != "!" {
        true
    } else {
        false
    };
    let operand = if binary_operation { "D" } else { "" };

    let mut arithmetic_vector: Vec<String> = vec![];

    if binary_operation {
        arithmetic_vector.extend(register_d_stack("pop"));
    }

    let next_pop = vec![
        "@SP".to_owned(),
        "AM=M-1".to_owned(), // decrement to top of stack and we go to new address
        format!("D={}{}M", operand, operator),
    ];

    arithmetic_vector.extend(next_pop);
    arithmetic_vector
}

fn write_comparison(operator: &str) -> Vec<String> {
    let mut logic_vector: Vec<String> = vec![];
    logic_vector.extend(register_d_stack("pop"));
    let next_pop = vec![
        "@SP".to_owned(),
        "AM=M-1".to_owned(),
        "D=D-M".to_owned(), // compare two numbers
        format!("D;{}", operator),
    ];
    logic_vector.extend(next_pop);
    logic_vector
}

// Branching commands --
// prior asm -> condition
fn write_label(label: &str) -> Vec<String> {
    // VM: label label
    vec![format!("({})", label)]
}

fn write_goto(label: &str) -> Vec<String> {
    // VM: goto label (unconditional)
    vec![format!("@{}", label), "0;JMP".to_owned()]
}

fn write_if_goto(label: &str) -> Vec<String> {
    // VM: if-goto label
    // i.e. jump if condition (condition is pushed previously-> look at stack)
    let mut if_goto_vec = vec![];
    // Pop out top stack's true/false
    if_goto_vec.extend(register_d_stack("pop"));
    // conditional jump
    let conditional_jump = vec![
        format!("@{}", label),
        "D;JNE".to_owned(), // jump when != 0
    ];
    if_goto_vec.extend(conditional_jump);
    if_goto_vec
}

fn write_function(name: &str, n_vars: &u16) -> Vec<String> {
    // VM: function functioName nVars
    // nVars -> pop out that many to LCL
    // functionName$label -> xxx.foo$bar / functionName -> xxx.foo
    // Name already includes fileName.foo
    let mut asm_code = write_label(name); // &(file_name.to_owned() + "." +

    for _ in 0..*n_vars {
        // Initalise local variables to zero
        asm_code.extend(write_push(
            &StackBaseMemory {
                segment: MemorySegment::Constant,
                index: 0,
            },
            "",
        ));
    }
    asm_code
}

fn write_return() -> Vec<String> {
    // VM: return
    let mut asm_code: Vec<String> = vec![];

    // end_frame = LCL // end_frame is a temp value - using R14
    let (end_frame, return_address) = ("@R14", "@R15");
    asm_code.extend(vec![
        "@LCL".to_owned(),
        "D=M".to_owned(), // Set pointer of LCL to D register
        end_frame.to_owned(),
        "M=D".to_owned(), // load that to the temp end_frame
    ]);

    // return_address = *(end_frame - 5) // temp value again - R15
    asm_code.extend(vec![
        "@5".to_owned(),
        // D is still end_frame above -> (end_frame - 5) [4 memory segments and return address on stack]
        "A=D-A".to_owned(),
        // Need to deference this address *()
        "D=M".to_owned(),
        // Into temp return_address
        return_address.to_owned(),
        "M=D".to_owned(),
    ]);

    asm_code.extend(register_d_stack("push")); // push return_address onto Stack

    // *ARG = pop()
    asm_code.extend(vec![
        // *ARG = pop() - copy onto arg 0
        "@ARG".to_owned(),
        "A=M".to_owned(), // go to address pointed to
        "M=D".to_owned(), // deference into D register
    ]);

    // SP = ARG + 1 // reposition SP -> everything below is now 'recycled'
    asm_code.extend(vec![
        "@ARG".to_owned(),
        "D=M+1".to_owned(),
        "@SP".to_owned(),
        "M=D".to_owned(),
    ]);

    // Restore segments: THAT *(end_frame-1), THIS *(end_frame-2), ARG *(end_frame-3), LCL *(end_frame-4)
    for (enum_index, segment_symbol) in ["@THAT", "@THIS", "@ARG", "@LCL"].iter().enumerate() {
        asm_code.extend(vec![
            // Grab pointer
            end_frame.to_owned(),
            "D=M".to_owned(),
            // Pointer offset
            format!("@{}", enum_index + 1),
            "A=D-A".to_owned(),
            // Deference pointer
            "D=M".to_owned(),
            // Reposition segments
            segment_symbol.to_string(),
            "M=D".to_owned(),
        ]);
    }

    // goto return_address
    asm_code.extend(vec![
        format!("{}", return_address),
        "A=M".to_owned(), // dereference (not included in normal go_to)
        "0;JMP".to_owned(),
    ]);

    asm_code
}

static mut CALL_COUNTER: u16 = 0;
fn write_calls(name: &str, n_args: &u16) -> Vec<String> {
    // VM: call functioName nArgs
    let mut asm_code: Vec<String> = vec![];

    // push returnAddress (label declared below)
    unsafe {
        // unsafe due to mutable static counter
        // return address - can't save to variable without it being awkward
        asm_code.push(format!("@{}.{}", name, CALL_COUNTER));
    };
    asm_code.push("D=A".to_owned()); // Push address onto d register
    asm_code.extend(register_d_stack("push")); // then push onto stack

    // push LCL, ARG, THIS, THAT
    for seg in ["LCL", "ARG", "THIS", "THAT"].iter() {
        asm_code.extend(vec![format!("@{}", seg), "D=M".to_owned()]); // Memory value onto D register
        asm_code.extend(register_d_stack("push")); // pushed to stack
    }

    // ARG = SP-5-nArgs
    let moved_arg: Vec<String> = vec![
        "@SP".to_owned(),
        "D=M".to_owned(),
        format!("@{}", 5 + n_args),
        "D=D-A".to_owned(),
        "@ARG".to_owned(),
        "M=D".to_owned(),
    ];
    asm_code.extend(moved_arg);
    // LCL = SP // reposition LCL
    let reset_local: Vec<String> = vec![
        "@SP".to_owned(),
        "D=M".to_owned(),
        "@LCL".to_owned(),
        "M=D".to_owned(),
    ];
    asm_code.extend(reset_local);

    // goto functionName
    asm_code.extend(write_goto(name));

    // return address label
    unsafe {
        asm_code.extend(write_label(&format!("{}.{}", name, CALL_COUNTER)));
        CALL_COUNTER += 1;
    };

    asm_code
}

fn write_push(command: &StackBaseMemory, file_name: &str) -> Vec<String> {
    let mut command_vector: Vec<String> = address_into_a_register(command, file_name);

    if command.segment == MemorySegment::Constant {
        command_vector.push("D=A".to_owned());
    } else {
        command_vector.push("D=M".to_owned());
    }
    command_vector.extend(register_d_stack("push"));
    command_vector
}

fn write_pop(command: &StackBaseMemory, file_name: &str) -> Vec<String> {
    // R13
    let temp_ram_slot = format!(
        "@R{}",
        StackBaseMemory::segment_base_index(&MemorySegment::GeneralPurpose)
    );

    let mut command_vector: Vec<String> = address_into_a_register(command, file_name);

    let store_as_temp: Vec<String> = vec![
        "D=A".to_owned(),
        temp_ram_slot.to_owned(), // Reg 13-15 are free to use - remember side effect of A
        "M=D".to_owned(),
    ];

    command_vector.extend(store_as_temp);
    command_vector.extend(register_d_stack("pop"));

    let save_to_memory = vec![
        temp_ram_slot.to_owned(), // Temp ram to A register
        "A=M".to_string(),        // dereference
        "M=D".to_string(),        // save to memory
    ];

    command_vector.extend(save_to_memory);
    command_vector
}

fn address_into_a_register(command: &StackBaseMemory, file_name: &str) -> Vec<String> {
    let seg_symbol_name = match command.segment {
        MemorySegment::Local => "LCL",
        MemorySegment::Argument => "ARG",
        MemorySegment::This => "THIS",
        MemorySegment::That => "THAT",
        MemorySegment::Constant => return vec![format!("@{}", command.index)],

        MemorySegment::Pointer | MemorySegment::Temp => {
            return vec![format!(
                "@{}",
                StackBaseMemory::segment_base_index(&command.segment) + command.index
            )]
        }
        MemorySegment::Static => return vec![format!("@{}.{}", file_name, command.index)],

        _ => panic!("Not updated?"),
    };
    let a_register = vec![
        format!("@{}", command.index),
        "D=A".to_owned(),
        format!("@{}", seg_symbol_name),
        "A=M+D".to_owned(),
    ];
    a_register
}

fn register_d_stack(push_or_pop: &str) -> Vec<String> {
    match push_or_pop {
        "push" => {
            // Push value onto stack
            return vec![
                // Place value into the next available address on stack (stack pointer)
                "@SP".to_owned(),
                "A=M".to_owned(),
                "M=D".to_owned(),
                // Increment SP
                "@SP".to_owned(),
                "M=M+1".to_owned(),
            ];
        }
        "pop" => {
            // Pop value off the stack to D register
            return vec![
                "@SP".to_owned(),
                "AM=M-1".to_owned(), // Decrement SP - Also change address
                "D=M".to_owned(),    // Store value in D
            ];
        }
        _ => panic!(
            "Invalid input on register d function - this shouldn't happen anyways as private"
        ),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn label_test() {
        let new_label = &("file".to_owned() + "." + "foo");
        assert_eq!(write_label(new_label), vec!["(file.foo)"])
    }

    #[test]
    fn push_command() {
        let push_const = StackBaseMemory {
            segment: MemorySegment::Constant,
            index: 10,
        };
        let file_name = "test";
        assert_eq!(
            write_push(&push_const, &file_name),
            vec!["@10", "D=A", "@SP", "A=M", "M=D", "@SP", "M=M+1"]
        );

        let push_non_const = StackBaseMemory {
            segment: MemorySegment::Static,
            index: 20,
        };
        assert_eq!(
            write_push(&push_non_const, &file_name),
            vec!["@test.20", "D=M", "@SP", "A=M", "M=D", "@SP", "M=M+1"]
        )
    }

    #[test]
    fn pop_command() {
        let pop_const = StackBaseMemory {
            segment: MemorySegment::Constant,
            index: 10,
        };
        let file_name = "test";
        assert_eq!(
            write_pop(&pop_const, &file_name),
            vec!["@10", "D=A", "@R13", "A=M", "M=D", "@SP", "AM=M-1", "D=M", "@R13", "A=M", "M=D"]
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
WritePushPop -> command (CPush / CPop), segment (string), indx(int) -> _
                        -> writes to output the assembly code of either CPush / CPop command
Close -> _ -> _ -> closes the output file
    more routines will be added to this module in proj 8.
*/
