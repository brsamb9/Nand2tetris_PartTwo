mod parser;
mod codeWriter;

#[macro_use]
extern crate lazy_static;

use std::env; //for args


fn main() {
    let args: Vec<String> = env::args().collect();
    
    let parsed_file = parser::parse(&args).unwrap();

    let file_name = &args[1];

    let mut output_vec: Vec<String> = vec![];
    for command in parsed_file{
        let comment_line = "// ".to_owned() + &command;
        output_vec.push(comment_line);
        let raw_instruction = parser::parse_current_command(&command).unwrap();

        let asm_code: Vec<String> = codeWriter::write_command(&raw_instruction, file_name);
        output_vec.extend(asm_code);
    }
    for line in output_vec{
        // >> to an output file
        println!{"{}", line};
    }

    
}
