
#[macro_use] extern crate lazy_static;
mod jack_tokenizer;
mod compilation_engine;
mod symbol_table;
mod vm_writer;

use std::env; //for args
use std::fs;


use jack_tokenizer::TokenError;
use compilation_engine::CompileError;
// use symbol_table::SymbolTableError;
use vm_writer::VmInstruError;

#[derive(Debug)]
enum ErrorTypes{
    TokenError(TokenError),
    CompileError(CompileError),
    VmInstruError(VmInstruError),
    
}
impl From<TokenError> for ErrorTypes {
    fn from(error: TokenError) -> Self {
        ErrorTypes::TokenError(error)
    }
}

impl From<CompileError> for ErrorTypes {
    fn from(error: CompileError) -> Self {
        ErrorTypes::CompileError(error)
    }
}
impl From<VmInstruError> for ErrorTypes {
    fn from(error: VmInstruError) -> Self {
        ErrorTypes::VmInstruError(error)
    }
}

fn main() -> Result<(), ErrorTypes> {
    // Although did few unit testing -> integration tests were done via CPUEmulator tool

    let file_or_directory: String = env::args().nth(1).expect("Incorrect argument input. E.g. ./program_name file/directory");

    let files = jack_tokenizer::parse_file_or_directory(&file_or_directory).unwrap();
    
    for file_name in files{
        // Grab tokens from file
        let mut tokenized = &mut jack_tokenizer::to_token_stream(&file_name)?;
        // start of compilation
        let compiled = compilation_engine::compile_class(&mut tokenized)?;

        let vm_instructions = vm_writer::write_vm_code(&compiled)?;
        

        let vm_file_name = format!("{}.vm", file_name.split(".").nth(0).unwrap());
        
        let mut contents = String::new();
        for vm in vm_instructions {    
            contents.push_str(&(vm.to_string() + "\n")[..]);
        }
        fs::write(vm_file_name, contents).unwrap();

    }
    Ok(())
}


