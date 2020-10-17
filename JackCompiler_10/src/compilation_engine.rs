/*
Parse Trees (recursive nature) -> e..g if (){ while () { } }
*/

use super::jack_tokenizer::{TokenTypes, Tokens};
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

#[derive(Debug)]
pub struct CompileError{
    pub error: String,
}
impl CompileError {
    fn new(err: String) -> Self {
        CompileError{error: err.to_owned()}
    }
}

lazy_static! {
    static ref SYM_TABLE: HashMap<&'static str, &'static str> = {
        let mut sym = HashMap::new();
        sym.insert("{", "OPEN_BRACE");
        sym.insert("}", "CLOSED_BRACE");
        sym.insert("[", "OPEN_SQUARED_BRACKET");
        sym.insert("]", "CLOSED_SQUARED_BRACKET");
        sym.insert("(", "OPEN_PARENTHESIS");
        sym.insert(")", "CLOSED_PARENTHESIS");
        sym.insert(":", "COLON");
        sym.insert(";", "SEMI-COLON");
        sym.insert(".", "PERIOD");
        sym.insert(",", "COMMA");
        sym.insert("<", "LESS_THAN");
        sym.insert(">", "MORE_THAN");
        sym.insert("=", "EQUALS");
        sym.insert("+", "PLUS");
        sym.insert("-", "MINUS");
        sym.insert("/", "DIVIDE");
        sym.insert("*", "MULTIPLY");
        sym
    };
}

// Global Indent related -> xml spacing
static mut INDENT: usize = 0;
// Tag functions w/ 'unsafe' code (due to indent)
fn base_tag(label: &String, xml_file: &mut File) {
    write!(xml_file, "{}<{}>\n", "  ".repeat(unsafe { INDENT }), label).unwrap();
    unsafe {
        INDENT += 1;
    }
}
fn end_tag(label: &String, xml_file: &mut File) {
    unsafe {
        INDENT -= 1;
    }
    write!(xml_file, "{}</{}>\n", "  ".repeat(unsafe { INDENT }), label).unwrap();
}

pub fn compile_next(
    token_tuple: (&String, &TokenTypes),
    xml_file: &mut File,
    tokenized: &mut Tokens,
) -> Result<(), CompileError> {
    // All feed into this one to continue recursive nature
    //let check_current = Tokens::peak_ahead(&tokenized, 0).unwrap();
    //println!("DEBUG (COMPILE_NEXT) -> {:?} __ {:?}", &token_tuple.0[..], &token_tuple.1);
    if token_tuple.1 == &TokenTypes::Keyword{
        match &token_tuple.0[..] {
            "class" => compile_class(token_tuple, xml_file, tokenized)?,
            "static" | "field" | "var" => compile_var_dec(token_tuple, xml_file, tokenized)?, // compiles a static / field declaration / var
            "method" | "function" | "constructor" => compile_subroutine(token_tuple, xml_file, tokenized)?, // compiles a complete method, function, or constructor
            //"var" => compile_var_dec(token_tuple, xml_file, tokenized),

            "let" | "if" | "while" | "do" | "return" => compile_statements(xml_file, tokenized)?,


            _ => return Err(CompileError::new(format!("Not yet done {:?}", token_tuple))),
        }
    } else {
        return Err(CompileError::new(format!("What did i miss? {:?}", token_tuple)));
    }

    Ok(())
}

fn compile_next_iter(xml_file: &mut File, tokenized: &mut Tokens) -> Result<(), CompileError>{
    if let Some((adv_token, adv_type)) = Tokens::advance(tokenized) {
        compile_next((&adv_token, &adv_type), xml_file, tokenized)?;
    } 
    Ok(())
}

// a set of compile_xxx methods, one for almost each non-terminal rule xxx

fn compile_statements(xml_file: &mut File, tokenized: &mut Tokens) -> Result<(), CompileError> {
    // code for compiling statements
    base_tag(&("statements").to_owned(), xml_file);
    
    loop { 
        match &Tokens::peak_ahead(tokenized, 0).unwrap().0[..] {
            "let" => compile_let( xml_file, tokenized)?,
            "if" => compile_if(xml_file, tokenized)?,
            "while" => compile_while( xml_file, tokenized)?, 
            "do" => compile_do( xml_file, tokenized)?,
            "return" => compile_return(xml_file, tokenized)?,
            _ => break,
            //return Err(CompileError::new(format!("Unexpected token in compile_statements: {}", token_tuple.0))),
        }
    }
    end_tag(&("statements").to_owned(), xml_file);
    Ok(())
}


fn compile_if(xml_file: &mut File, tokenized: &mut Tokens) -> Result<(), CompileError> {
    let current_token_tuple = Tokens::peak_ahead(tokenized, 0).unwrap();

    base_tag(&("if".to_owned() + "Statement"), xml_file);
    // if
    write_xml_line(current_token_tuple.0, current_token_tuple.1, xml_file);

    // Ruleset.. 

    end_tag(&("if".to_owned() + "Statement"), xml_file);
    Ok(())
}


fn compile_while( xml_file: &mut File, tokenized: &mut Tokens) -> Result<(), CompileError> {
    let current_token_tuple = Tokens::peak_ahead(tokenized, 0).unwrap();
    base_tag(&("while".to_owned() + "Statement"), xml_file);
    // while
    write_xml_line(current_token_tuple.0, current_token_tuple.1, xml_file);


    end_tag(&("while".to_owned() + "Statement"), xml_file);

    Ok(())
}

fn compile_expression(xml_file: &mut File, tokenized: &mut Tokens){
    base_tag(&("expression".to_owned()), xml_file);

    // compile term

    // while operator -> operator and another term... 

    end_tag(&("expression".to_owned()), xml_file);
}


fn compile_term(xml_file: &mut File, tokenized: &mut Tokens) -> Result<(), CompileError>{
    // IntegerConstant | StringConstant | Keyword | varName | 
    // varName '[' expression ']' | subroutine | '(' expression ')' | operationTerm
    base_tag(&("term".to_owned()), xml_file);
    loop {
        let (token, token_type) = Tokens::advance(tokenized).unwrap();

        match token_type {
            TokenTypes::Identifier => {
                write_xml_line(&token, &token_type, xml_file);
                if Tokens::peak_ahead(tokenized, 1).unwrap() == (&("[".to_owned()), &TokenTypes::Symbol) {
                    // varName '[' expression ']'
                    let (token, token_type) = Tokens::advance(tokenized).unwrap();
                    write_xml_line(&token, &token_type, xml_file);
                    compile_expression(xml_file, tokenized);
                    let (token, token_type) = Tokens::advance(tokenized).unwrap();
                    write_xml_line(&token, &token_type, xml_file);
                }
            } 
            TokenTypes::Symbol => {
                let current_symbol = SYM_TABLE[&token[..]];
                match current_symbol {
                    "OPEN_PARATHENSIS" => {
                        /* '(' expression ')' */
                        symbol_check_and_xml_line("OPEN_PARATHENSIS", &token, &token_type, xml_file)?;
                        compile_expression(xml_file, tokenized);
                        let temp_tup =Tokens::peak_ahead(tokenized, 0).unwrap();
                        symbol_check_and_xml_line("CLOSED_PARATHENSIS", temp_tup.0, temp_tup.1, xml_file)?;
                    }
                    "PLUS" | "MINUS" | "DIVIDE" | "MULTIPLY" | "EQUAL" => {
                        /* operationTerm */
                    }
                    "OPEN_BRACKET" => {
                        /* varName '[' expression ']' */
                    }
                    "PERIOD" => { 
                        /*Subroutine*/
                    }
                    _ => {return Err(CompileError::new("Unexpected symbol in compile_term".to_owned()))}
                }
            }



            TokenTypes::StringConstant | TokenTypes::IntegerConstant => {}  
            TokenTypes::Keyword => {}
            TokenTypes::InferType => return Err(CompileError::new("Unexpected infertype in compile_term".to_owned())),
        }

        if Tokens::peak_ahead(tokenized, 1).unwrap() == (&(";".to_owned()), &TokenTypes::Symbol) {
            break;
        }
    }
    end_tag(&("term".to_owned()), xml_file);
    Ok(())
}

fn compile_let( xml_file: &mut File, tokenized: &mut Tokens) -> Result<(), CompileError> {
    let current_token_tuple = Tokens::peak_ahead(tokenized, 0).unwrap();
    base_tag(&("let".to_owned() + "Statement"), xml_file);
    // let
    write_xml_line(current_token_tuple.0, current_token_tuple.1, xml_file);

    // letStatements => 'let' varName ?('[', expression, ']')? '=' expression ';'
    let let_rules = [
            TokenTypes::Identifier, // varName
            TokenTypes::InferType,  // ?('[', expression, ']')? = OR =
            TokenTypes::InferType,  // Expression
            TokenTypes::Symbol,     // ;
            ].iter();


    for (idx, rule) in let_rules.enumerate() {
        let (adv_token, adv_type) = Tokens::advance(tokenized).unwrap();
        println!("Debug: let -> {:?} {:?}", adv_token, adv_type);
        match (idx, rule) {
            

            _ => return Err(CompileError::new(format!("Error in compile_let => {} {:?}, expected: {:?}", adv_token,adv_type, rule))),
        }
    }

    end_tag(&("let".to_owned() + "Statement"), xml_file);
    Ok(())
}


fn compile_do(xml_file: &mut File, tokenized: &mut Tokens) -> Result<(), CompileError> {
    let current_token_tuple = Tokens::peak_ahead(tokenized, 0).unwrap();
    base_tag(&("do".to_owned() + "Statement"), xml_file);
    // do
    write_xml_line(current_token_tuple.0, current_token_tuple.1, xml_file);


    end_tag(&("do".to_owned() + "Statement"), xml_file);
    Ok(())
}


fn compile_return( xml_file: &mut File, tokenized: &mut Tokens) -> Result<(), CompileError> {
    let current_token_tuple = Tokens::peak_ahead(tokenized, 0).unwrap();
    base_tag(&("return".to_owned() + "Statement"), xml_file);
    // return 
    write_xml_line(current_token_tuple.0, current_token_tuple.1, xml_file);
    
    // ; or identifier; or identifier, identifier .. ;



    end_tag(&("return".to_owned() + "Statement"), xml_file);
    Ok(())
}


fn ruleset(rule: &str) -> Result<Vec<TokenTypes>, CompileError> {
    match rule {
        // remainer of class ruleset
        "class" => Ok(vec![
            TokenTypes::Identifier, // Name of class
            TokenTypes::Symbol,     /* { */
            TokenTypes::InferType,  /*BODY*/
            TokenTypes::Symbol,     /* } */
        ]),
        "static" | "field" | "var" => Ok(vec![
            TokenTypes::Keyword,    // type
            TokenTypes::Identifier, // Name of variable
            TokenTypes::Symbol,  // , name...; or ; -> compile_multi_vars then
        ]),
        "function" | "method" | "constructor" => Ok(vec![
            TokenTypes::Keyword,    // type
            TokenTypes::Identifier, // name of function
            TokenTypes::Symbol,     // (
            TokenTypes::InferType,  // possible paramter_list
            TokenTypes::Symbol,     // )
            TokenTypes::Symbol,     // {
            TokenTypes::InferType,  // possible function_body
            TokenTypes::Keyword,    // return
            TokenTypes::InferType,  // either ; or variable names and ;
            TokenTypes::Symbol,     // }
        ]),
        "if" | "while" => Ok(vec![
            // whileStatements =>   'while' '(' expression ')' '{' statements '}'
            // ifStatement =>       'if'    '(' expression ')' '{' statements '}'
            TokenTypes::Symbol,    // (
            TokenTypes::InferType, // expression / or identifier -> e.g. x < 3, x = 2, x, or x + 3 < 4
            TokenTypes::Symbol,    // )
            TokenTypes::Symbol,    // {
            TokenTypes::InferType, // statements;
            TokenTypes::Symbol,    // }
        ]),
        //"let" => Ok()
        "return" => Ok(vec![
            // return statement -> 'return', ';' OR 'identifier', ';' OR ', identifier', ';'
            TokenTypes::InferType, // ; or identifier; or identifier, identifier .. ;
        ]),
        _ => return Err(CompileError::new("Ruleset not recognized/designed".to_owned())),
    }
}

fn symbol_check_and_xml_line(
    desired_symbol: &str,
    adv_token: &String,
    adv_type: &TokenTypes,
    xml_file: &mut File,
) -> Result<(), CompileError> {
    let curr_symbol = &adv_token[..]; 
    if SYM_TABLE.contains_key(curr_symbol){
        if SYM_TABLE[curr_symbol] == desired_symbol {
            write_xml_line(&adv_token, &adv_type, xml_file);
            return Ok(());
        }
    }
    return Err(CompileError::new(format!("SymCheck => Different symbol given than expected -> {:?} <-> {:?} != {:?}",
        adv_token, adv_type, desired_symbol
    )));
}

fn compile_class(token_tuple: (&String, &TokenTypes), xml_file: &mut File, tokenized: &mut Tokens) -> Result<(), CompileError> {
    base_tag(token_tuple.0, xml_file);
    // Normal Tag
    write_xml_line(token_tuple.0, token_tuple.1, xml_file);

    let class_rules = ruleset(&token_tuple.0[..])?;
    for (idx, rule) in class_rules.iter().enumerate() {
        let (adv_token, adv_type) = Tokens::advance(tokenized).unwrap();
        println!("Debug: Class -> {:?} {:?}", adv_token, adv_type);
        match (idx, rule) {
            (0, &TokenTypes::Identifier) => write_xml_line(&adv_token, &adv_type, xml_file),
            (1, &TokenTypes::Symbol) => {
                symbol_check_and_xml_line("OPEN_BRACE", &adv_token, &adv_type, xml_file)?
            }
            (2, &TokenTypes::InferType) => {
                // If no body ->
                if adv_type == TokenTypes::Symbol {
                    symbol_check_and_xml_line("CLOSED_BRACE", &adv_token, &adv_type, xml_file)?;
                    break; // break from loop
                } else {
                    compile_next((&adv_token, &adv_type), xml_file, tokenized)?;
                }
            }
            (3, &TokenTypes::Symbol) => {
                symbol_check_and_xml_line("CLOSED_BRACE", &adv_token, &adv_type, xml_file)?
            }

            _ => { return Err(CompileError::new(
                format!("compile_class => No matches -> {:?} <-> {:?}: @ iter {} -> {:?}", adv_token, adv_type, idx, rule)));
            }
        }
    }
    // Check before tag
    compile_next_iter(xml_file, tokenized)?;
    end_tag(token_tuple.0, xml_file);
    Ok(())
}

fn compile_var_dec(
    token_tuple: (&String, &TokenTypes),
    xml_file: &mut File,
    tokenized: &mut Tokens,
) -> Result<(), CompileError> {
    // Compiles a static or field declaration
    // Declaration tag
    let label = match &token_tuple.0[..] {
        "static" | "field" => ("classVar".to_owned() + "Dec"),
        "var" => ("var".to_owned() + "Dec"),
        _ =>  return Err(CompileError::new("Invalid entry at tag_name".to_owned())),
    };

    base_tag(&label, xml_file);
    // name type identifer => keyword, keyword, identifier
    // Static / field
    write_xml_line(token_tuple.0, token_tuple.1, xml_file);
    println!("Debug: SubRout -> {:?}", token_tuple);
    let var_dec_rules = ruleset(&token_tuple.0[..])?;
    for (idx, rule) in var_dec_rules.iter().enumerate() {
        let (adv_token, adv_type) = Tokens::advance(tokenized).unwrap();
        println!("Debug: VarDec -> {:?} {:?}", adv_token, adv_type);
        match (idx, rule) {
            (0, TokenTypes::Keyword) => write_xml_line(&adv_token, &adv_type, xml_file), // type
            (1, TokenTypes::Identifier) => write_xml_line(&adv_token, &adv_type, xml_file), // name of static/field
            (2, TokenTypes::Symbol) => match SYM_TABLE[&adv_token[..]] {
                "SEMI-COLON" => {
                    symbol_check_and_xml_line("SEMI-COLON", &adv_token, &adv_type, xml_file)?;
                }
                "COMMA" => {
                    symbol_check_and_xml_line("COMMA", &adv_token, &adv_type, xml_file)?;
                    multiple_variables(xml_file, tokenized)?;
                }
                _ => return Err(CompileError::new(format!( "@Var_dec => Current: Token {:?}, type: {:?};\n@ iter {}, rule: {:?}", adv_token, adv_type, idx, rule
                ))),

            },
            _ => return Err(CompileError::new(format!(
                "@Var_dec => No matches: Current: Token {:?}, type: {:?}; @ iter {}, rule: {:?}", adv_token, adv_type, idx, rule
            ))),
        }
    }
    
    end_tag(&label, xml_file); // End tag before check next
    compile_next_iter(xml_file, tokenized)?;
    Ok(())
}

fn compile_subroutine(
    token_tuple: (&String, &TokenTypes),
    xml_file: &mut File,
    tokenized: &mut Tokens,
) -> Result<(), CompileError> {
    // Compiles function / method / constructor => type nameVar(possible PARAMETERS) { BODY };
    // Declaration tag
    base_tag(&("subroutine".to_owned() + "Dec"), xml_file);
    // function/method/constructor Tag tag
    println!("Debug: SubRout -> {:?}", token_tuple);
    write_xml_line(token_tuple.0, token_tuple.1, xml_file);

    let mut skip_one_iteration = false;
    let subroutine_rules = ruleset(&token_tuple.0[..])?;
    for (idx, rule) in subroutine_rules.iter().enumerate() {
        if skip_one_iteration {
            // Skip as no parameters given -> skip iteration to sync again
            skip_one_iteration = false;
            continue;
        }
        let (adv_token, adv_type) = Tokens::advance(tokenized).unwrap();
        println!("Debug: SubRout -> {:?} {:?}", adv_token, adv_type);
        match (idx, rule) {
            // type
            (0, &TokenTypes::Keyword) => write_xml_line(&adv_token, &adv_type, xml_file),
            // identifier
            (1, &TokenTypes::Identifier) => write_xml_line(&adv_token, &adv_type, xml_file),
            // symbol (
            (2, &TokenTypes::Symbol) => {
                symbol_check_and_xml_line("OPEN_PARENTHESIS", &adv_token, &adv_type, xml_file)?;
            }
            (3, &TokenTypes::InferType) => {
                // Possible Parameter list -> could return empty list, i.e. this token is ')'
                compile_parameter_list((&adv_token, &adv_type), xml_file, tokenized)?;
                if adv_type == TokenTypes::Symbol {
                    // if empty, the adv_type will be symbol - loops out of sync now
                    symbol_check_and_xml_line(
                        "CLOSED_PARENTHESIS",
                        &adv_token,
                        &adv_type,
                        xml_file,
                    )?;
                    skip_one_iteration = true;
                }
            }
            // )
            (4, &TokenTypes::Symbol) => {
                symbol_check_and_xml_line("CLOSED_PARENTHESIS", &adv_token, &adv_type, xml_file)?;
            }
            // {
            (5, &TokenTypes::Symbol) => {
                symbol_check_and_xml_line("OPEN_BRACE", &adv_token, &adv_type, xml_file)?;
            }
            (6, &TokenTypes::InferType) => {
                // BODY
                base_tag(&("subroutine".to_owned() + "Body"), xml_file);
                if adv_type != TokenTypes::Symbol {
                    compile_next((&adv_token, &adv_type), xml_file, tokenized)?;
                }
                end_tag(&("subroutine".to_owned() + "Body"), xml_file);

                if adv_type == TokenTypes::Symbol {
                    // if empty, the adv_type will be symbol - loops out of sync now
                    symbol_check_and_xml_line("CLOSED_BRACE", &adv_token, &adv_type, xml_file)?;
                    break;
                }
            }
            // }
            (7, &TokenTypes::Symbol) => {
                symbol_check_and_xml_line("CLOSED_BRACE", &adv_token, &adv_type, xml_file)?;
            }
            _ => return Err(CompileError::new(format!( "compile_subroutine => No matches -> {:?} <-> {:?}: @ iter {} -> {:?}",
            adv_token, adv_type, idx, rule))),
        }
    }
    // Declaration tag
    compile_next_iter(xml_file, tokenized)?;
    end_tag(&("subroutine".to_owned() + "Dec"), xml_file);
    Ok(())
}

fn compile_parameter_list(
    token_tuple: (&String, &TokenTypes),
    xml_file: &mut File,
    tokenized: &mut Tokens,
) -> Result<(), CompileError> {
    base_tag(&("parameterList".to_owned()), xml_file);
    // check if empty ->
    let (adv_token, adv_type) = token_tuple;
    println!("Debug: ParaList -> {:?} {:?}", adv_token, adv_type);
    if adv_type == &TokenTypes::Symbol {
        symbol_check_and_xml_line("CLOSED_PARENTHESIS", &adv_token, &adv_type, xml_file)?;
    } else if adv_type == &TokenTypes::Identifier {
        write_xml_line(&adv_token, &adv_type, xml_file);
        multiple_variables(xml_file, tokenized)?;
    }
    end_tag(&("parameterList".to_owned()), xml_file);
    Ok(())
}

fn multiple_variables(xml_file: &mut File, tokenized: &mut Tokens) -> Result<(), CompileError> {
    loop {
        let (adv_token, adv_type) = Tokens::advance(tokenized).unwrap();
        if adv_type == TokenTypes::Identifier {
            write_xml_line(&adv_token, &adv_type, xml_file); // Name of variable
        } else if adv_type == TokenTypes::Symbol {
            let current_symbol = SYM_TABLE[&adv_token[..]];
            match current_symbol {
                "COMMA" => symbol_check_and_xml_line("COMMA", &adv_token, &adv_type, xml_file)?,
                "SEMI-COLON" => {
                    symbol_check_and_xml_line("SEMI-COLON", &adv_token, &adv_type, xml_file)?;
                    break;
                }
                _ => return Err(CompileError::new(format!("Compile_multi_var -> weird symbol = {:?} {:?} ",
                adv_token, adv_type))),
            }
        } else {
            return Err(CompileError::new(format!("@Multi_var -> Not expected in infinite loop = {:?} {:?} ",
            adv_token, adv_type)));
        }
    }

    Ok(())
}



fn write_xml_line(token: &String, token_type: &TokenTypes, xml_file: &mut File) {
    let string_token = TokenTypes::string_token_type(&token_type);
    write!(
        xml_file,
        "{}<{}> {} </{}>\n",
        " ".repeat(unsafe { INDENT }),
        string_token,
        token,
        string_token
    )
    .unwrap();
}

mod test {
    use super::*;
    use super::super::jack_tokenizer::to_token_stream;
    
    #[test]
    fn compile_driver_tests() {
        
        let file = "/home/brs/Documents/JackCompiler/tests/ArrayTest/Main.jack".to_owned();
        let mut tokenized = to_token_stream(&file).unwrap();

        let mut xml_file =
            File::create("/home/brs/Documents/JackCompiler/tests/other/Array_Tests.xml").unwrap();
        let ttup = Tokens::advance(&mut tokenized).unwrap();

        compile_next((&ttup.0, &ttup.1), &mut xml_file, &mut tokenized).unwrap()
    }

    #[test]
    fn compare_tokens_types() {
        assert!(&TokenTypes::Identifier != &TokenTypes::Keyword);
        assert!(&TokenTypes::Keyword == &TokenTypes::Keyword);
    }
    #[test]
    fn compile_expressionless_main() {
        let mut xml_file = File::create(
            "/home/brs/Documents/JackCompiler/tests/other/expressionless_square_test.xml",
        )
        .unwrap();

        let file =
            "/home/brs/Documents/nand2tetris/projects/10/ExpressionLessSquare/Main.jack".to_owned();
        let mut tokenized = to_token_stream(&file).unwrap();

        while let Some((token, token_type)) = &Tokens::advance(&mut tokenized) {
            compile_next((token, token_type), &mut xml_file, &mut tokenized).unwrap();
        }
    }
}
