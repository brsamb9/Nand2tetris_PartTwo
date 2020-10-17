mod code_writer;
mod parser;

use std::env; //for args

fn main() {
    // Although did few unit testing -> integration tests were done via CPUEmulator tool

    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Incorrect argument input. E.g. ./program_name file_or_directory_name")
    };

    let file_or_directory_name = &args[1];
    // .vm files
    let file_names = parser::relevant_files(file_or_directory_name);

    // Output (prints at the end for user to redirect with initial required bootstrap
        // could have simply print out lines rather than storing
    let mut asm_code_output: Vec<String> = code_writer::write_init();

    // Loop over vm files -> into a single asm file regardless
    for file in file_names {
        let file_name = if file.contains("/") {
            file.split("/").last().unwrap().to_owned()
        } else {
            file.to_owned()
        };
        // Go over parsed commands
        for command in parser::parse_file(&file).unwrap() {
            let comment_line = "// ".to_owned() + &command;
            asm_code_output.push(comment_line);

            let raw_instruction = parser::parse_current_command(&command).unwrap();
            let file_name_without_extension = file_name.split(".vm").nth(0).unwrap();
            let asm_code: Vec<String> =
                code_writer::write_command(&raw_instruction, file_name_without_extension);

            asm_code_output.extend(asm_code);
        }
    }

    for line in asm_code_output {
        // User to redirect: > to an output file
        println! {"{}", line};
    }
}
