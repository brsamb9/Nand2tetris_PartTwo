
#[macro_use] extern crate lazy_static;
mod jack_tokenizer;
mod compilation_engine;

use std::env; //for args
use std::fs::File;
use jack_tokenizer::{Tokens, TokenError};
use compilation_engine::CompileError;

#[derive(Debug)]
enum ErrorTypes{
    TokenError(TokenError),
    CompileError(CompileError),
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


fn main() -> Result<(), ErrorTypes> {
    // Although did few unit testing -> integration tests were done via CPUEmulator tool

    let file_or_directory: String = env::args().nth(1).expect("Incorrect argument input. E.g. ./program_name file/directory");

    let files = jack_tokenizer::parse_file_or_directory(&file_or_directory).unwrap();
    
    for file in files{
        // Grab tokens from file
        let tokenized = &mut jack_tokenizer::to_token_stream(&file).unwrap();
        
        // Open xml_file
        let mut xml_file = File::create(
            format!("{}_test.xml", &file.strip_suffix(".jack").unwrap()).to_owned()
        ).unwrap();

        // parser
        while let Some((token, token_type)) = &Tokens::advance(tokenized) {
            compilation_engine::compile_next((token, token_type), &mut xml_file, tokenized)?;
        }
    }
    Ok(())
}