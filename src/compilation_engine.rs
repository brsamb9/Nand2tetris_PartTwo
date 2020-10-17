/*
Parse Trees (recursive nature) -> e..g if (){ while () { } }
*/

use super::jack_tokenizer::{TokenTypes, Tokens};
use super::vm_writer::VmInstruction;

#[derive(Debug)]
pub struct CompileError {
    pub error: String,
}
impl CompileError {
    fn new(err: &str, tokenized: &mut Tokens) -> Self {
        CompileError {
            error: format!("{}-> Got: {:?}", err, tokenized.token_stream[0]),
        }
    }
}

//https://stackoverflow.com/questions/36167160/how-do-i-express-mutually-recursive-data-structures-in-safe-rust
#[derive(Debug, PartialEq)]
pub struct Parsed<T> {
    pub data: T,
    pub nested: Vec<Parsed<T>>,
}
impl<T> Parsed<T> {
    fn new(data: T) -> Parsed<T> {
        Parsed {
            data: data,
            nested: vec![],
        }
    }

    fn add_subroutine(&mut self, child: Parsed<T>) {
        self.nested.push(child);
    }

    pub fn number_of_subroutines(&self) -> usize {
        self.nested.len()
    }
}

#[derive(Debug, PartialEq)]
pub struct ProgramBlock {
    pub name: String,
    pub subroutine_type: Block,
    pub declar_var: Vec<DeclareVariable>,
    pub body_statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Block {
    Class,
    Constructor,
    Method,
    Function,
}

#[derive(Debug, PartialEq, Clone)]
pub struct DeclareVariable {
    pub name: String,
    pub name_type: VarTypes,
    pub kind: VarKinds,
}
#[derive(Debug, PartialEq, Clone)]
pub enum VarTypes {
    Int,
    Float,
    Boolean,
    Char,
    String,
    Void,
    Class(String),
}
impl VarTypes {
    fn string_to_vartype(s: String) -> Self {
        match &s[..] {
            "int" => VarTypes::Int,
            "float" => VarTypes::Float,
            "boolean" => VarTypes::Boolean,
            "char" => VarTypes::Char,
            "String" => VarTypes::String,
            "void" => VarTypes::Void,
            // Works for error-free jack code
            _s => VarTypes::Class(_s.to_string()),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum VarKinds {
    Static,
    Field, // field == this
    Local, // local/var == local
    Argument,
}
impl VarKinds {
    fn string_to_varkind(s: String) -> Self {
        match &s[..] {
            "static" => VarKinds::Static,
            "field" => VarKinds::Field,
            "var" => VarKinds::Local,
            // Function arguments -> fine for error-free jack code
            _ => VarKinds::Argument,
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Let {
        var_name: String,
        optional_index: Option<Expression>,
        expression: Expression,
    },
    If {
        condition: Expression,
        if_body: Vec<Statement>,
        else_body: Option<Vec<Statement>>,
    },
    While {
        condition: Expression,
        while_body: Vec<Statement>,
    },
    Do(CallSubroutine),
    Return(Option<Expression>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct CallSubroutine {
    pub parent_name: Option<String>,
    pub subroutine_name: String,
    pub parameters: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression(pub Vec<Terms>);

// // https://doc.rust-lang.org/std/iter/trait.IntoIterator.html
impl IntoIterator for Expression {
    type Item = Terms;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Terms {
    IntegerConstant(u16),
    StringConstant(String),
    KeywordConstant(KeywordConstant),
    VarName(String),
    Bracket(String, Expression),
    Parenthesis(Expression),
    CallSubroutine(CallSubroutine),
    Op(Op),
}
#[derive(Debug, PartialEq, Clone)]
pub enum KeywordConstant {
    True,
    False,
    Null,
    This,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Op {
    // Binary
    Plus,
    Minus,
    Divide,
    Multiply,
    Eq,
    And,
    Or,
    Lt,
    Gt,
    // Unary
    Neg,
    Not,
    // "+" | "-" | "/" | "*" | "=" | "&" | "|" | "<" | ">" | "~"
}
impl From<&Op> for VmInstruction {
    fn from(convert: &Op) -> Self {
        match convert {
            // Binary
            Op::Plus => VmInstruction::Add,
            Op::Minus => VmInstruction::Sub,
            Op::Divide => VmInstruction::Call("Math.divide".to_owned(), 2),
            Op::Multiply => VmInstruction::Call("Math.multiply".to_owned(), 2),
            Op::Eq => VmInstruction::Eq,
            Op::And => VmInstruction::And,
            Op::Or => VmInstruction::Or,
            Op::Lt => VmInstruction::Lt,
            Op::Gt => VmInstruction::Gt,
            // Unary
            Op::Neg => VmInstruction::Neg,
            Op::Not => VmInstruction::Not,
        }
    }
}
impl Op {
    fn string_to_binary_op(s: String) -> Terms {
        match &s[..] {
            "+" => Terms::Op(Op::Plus),
            "-" => Terms::Op(Op::Minus),
            "/" => Terms::Op(Op::Divide),
            "*" => Terms::Op(Op::Multiply),
            "=" => Terms::Op(Op::Eq),
            "&" => Terms::Op(Op::And),
            "|" => Terms::Op(Op::Or),
            "<" => Terms::Op(Op::Lt),
            ">" => Terms::Op(Op::Gt),
            _ => panic!(format!("Not a binary op -> {}", s)),
        }
    }
    fn string_to_unary_op(s: String) -> Terms {
        match &s[..] {
            "-" => Terms::Op(Op::Neg),
            "~" => Terms::Op(Op::Not),
            _ => panic!(format!("Not an unary op -> {}", s)),
        }
    }
}

type CompileCheck = Result<(), CompileError>;
type VarDeclaration = Result<Vec<DeclareVariable>, CompileError>;
type StatementReturn = Result<Statement, CompileError>;

pub fn compile_class(tokenized: &mut Tokens) -> Result<Parsed<ProgramBlock>, CompileError> {
    let tokenized = tokenized.into_iter();

    // class
    advance_check(tokenized, (Some(&["class"]), Some(&[TokenTypes::Keyword])))?;
    // class name
    let parent_name = advance_check(tokenized, (None, Some(&[TokenTypes::Identifier])))?.0;

    // {
    advance_check(tokenized, (Some(&["{"]), Some(&[TokenTypes::Symbol])))?;

    // Body or }
    let mut class_var_declaration: Vec<DeclareVariable> = Vec::new();
    while let Some((token, _)) = tokenized.peek() {
        match &token[..] {
            "static" | "field" => {
                class_var_declaration.extend(compile_class_var_dec(tokenized)?);
            }
            _ => break,
        };
    }

    let mut class_block = Parsed::new(ProgramBlock {
        name: parent_name,
        subroutine_type: Block::Class,
        declar_var: class_var_declaration,
        body_statements: vec![], // need a better idea for subroutines in class section here
    });

    while let Some((token, _)) = tokenized.peek() {
        match &token[..] {
            "constructor" | "function" | "method" => {
                compile_subroutine(tokenized, &mut class_block)?;
            }
            _ => break,
        }
    }

    advance_check(tokenized, (Some(&["}"]), Some(&[TokenTypes::Symbol])))?;

    Ok(class_block)
}

fn advance_check(
    tokenized: &mut Tokens,
    compare_tuple: (Option<&[&str]>, Option<&[TokenTypes]>),
) -> Result<(String, TokenTypes), CompileError> {
    // Check
    let token_tup = tokenized.peek().unwrap();

    if let Some(to_check) = compare_tuple.0 {
        if !to_check.iter().any(|s| *s == &token_tup.0[..]) {
            return Err(CompileError::new(
                &format!("Invalid token {:?}", to_check)[..],
                tokenized,
            ));
        }
    }
    if let Some(to_check) = compare_tuple.1 {
        if !to_check.iter().any(|s| s == &token_tup.1) {
            return Err(CompileError::new(
                &format!("Invalid type: {:?} ", to_check)[..],
                tokenized,
            ));
        }
    }
    // Push
    let token_tuple = tokenized.next().unwrap();
    Ok(token_tuple)
}

fn compile_class_var_dec(tokenized: &mut Tokens) -> VarDeclaration {
    // Compiles: [Static / field / var] type name ... ;

    let kind = VarKinds::string_to_varkind(
        advance_check(tokenized, (None, Some(&[TokenTypes::Keyword])))?.0,
    );
    // subroutine Type
    let name_type = VarTypes::string_to_vartype(
        advance_check(
            tokenized,
            (None, Some(&[TokenTypes::Keyword, TokenTypes::Identifier])),
        )?
        .0,
    );

    // subroutine_name -> ", moreNames.. ;", ";"
    let subroutine_names = compile_name_list(tokenized)?;

    let mut declaration_vector: Vec<DeclareVariable> = Vec::new();
    for name in subroutine_names {
        declaration_vector.push(DeclareVariable {
            name,
            name_type: name_type.clone(),
            kind,
        })
    }
    advance_check(tokenized, (Some(&[";"]), None))?;
    Ok(declaration_vector)
}

fn compile_subroutine(
    tokenized: &mut Tokens,
    class_block: &mut Parsed<ProgramBlock>,
) -> CompileCheck {
    // Compiles: [function / method / constructor] type nameVar(possible PARAMETERS) { BODY };
    let subroutine_string = advance_check(
        tokenized,
        (
            Some(&["constructor", "method", "function"]),
            Some(&[TokenTypes::Keyword]),
        ),
    )?
    .0;
    let subroutine_type = match &subroutine_string[..] {
        "constructor" => Block::Constructor,
        "method" => Block::Method,
        "function" => Block::Function,
        _ => return Err(CompileError::new("Shouldn't happen", tokenized)),
    };

    // Type - TODO ensure return type is true
    // let return_type = VarTypes::string_to_vartype(
    advance_check(
        tokenized,
        (None, Some(&[TokenTypes::Keyword, TokenTypes::Identifier])),
    )?
    .0;
    // );

    // identifier
    let subroutine_name = advance_check(tokenized, (None, Some(&[TokenTypes::Identifier])))?.0;

    // need to sort out declarations and body for subroutines ( and all the layers underneath! - fun times)
    let mut subroutine_declar: Vec<DeclareVariable> = Vec::new();

    if subroutine_type == Block::Method {
        subroutine_declar.push(DeclareVariable {
            name: "this".to_owned(),
            name_type: VarTypes::Class(class_block.data.name.clone()),
            kind: VarKinds::Argument,
        });
    }

    // (
    advance_check(tokenized, (Some(&["("]), Some(&[TokenTypes::Symbol])))?;
    // Possible Parameter list -> could return empty list
    subroutine_declar.extend(compile_parameter_list(tokenized)?);

    // )
    advance_check(tokenized, (Some(&[")"]), Some(&[TokenTypes::Symbol])))?;

    // BODY
    // {
    advance_check(tokenized, (Some(&["{"]), Some(&[TokenTypes::Symbol])))?;

    while "var" == tokenized.peek().unwrap().0 {
        subroutine_declar.extend(compile_var_dec(tokenized)?);
    }
    // while, if, do, return, etc
    let subroutine_body = compile_statements(tokenized)?;

    // }
    advance_check(tokenized, (Some(&["}"]), Some(&[TokenTypes::Symbol])))?;

    Parsed::add_subroutine(
        class_block,
        Parsed::new(ProgramBlock {
            name: subroutine_name,
            subroutine_type: subroutine_type,
            declar_var: subroutine_declar,
            body_statements: subroutine_body,
        }),
    );

    Ok(())
}

fn compile_statements(tokenized: &mut Tokens) -> Result<Vec<Statement>, CompileError> {
    // Body vector as a mutable reference as possible multiple reentries to this function
    let mut statements: Vec<Statement> = Vec::new();

    loop {
        let statement = match &tokenized.peek().unwrap().0[..] {
            "let" => compile_let(tokenized)?,
            "if" => compile_if(tokenized)?,
            "while" => compile_while(tokenized)?,
            "do" => compile_do(tokenized)?,
            "return" => compile_return(tokenized)?,
            _ => break,
        };
        statements.push(statement);
        // subroutine_body.push(Body(StatementOrExpression::Statement(statement)));
    }

    Ok(statements)
}

fn compile_if(tokenized: &mut Tokens) -> StatementReturn {
    // if (expression) {statements} "else"
    advance_check(tokenized, (Some(&["if"]), Some(&[TokenTypes::Keyword])))?;

    advance_check(tokenized, (Some(&["("]), None))?;
    let condition: Expression = compile_expression(tokenized)?;
    advance_check(tokenized, (Some(&[")"]), None))?;

    advance_check(tokenized, (Some(&["{"]), None))?;
    let if_body: Vec<Statement> = compile_statements(tokenized)?;
    advance_check(tokenized, (Some(&["}"]), None))?;

    let mut else_body = None;
    if &tokenized.peek().unwrap().0[..] == "else" {
        advance_check(tokenized, (Some(&["else"]), Some(&[TokenTypes::Keyword])))?;
        advance_check(tokenized, (Some(&["{"]), None))?;
        else_body = Some(compile_statements(tokenized)?);
        advance_check(tokenized, (Some(&["}"]), None))?;
    }

    Ok(Statement::If {
        condition,
        if_body,
        else_body,
    })
}

fn compile_while(tokenized: &mut Tokens) -> StatementReturn {
    // 'while' '(' 'expression' ')' '{' statements '}'
    advance_check(tokenized, (Some(&["while"]), Some(&[TokenTypes::Keyword])))?;

    advance_check(tokenized, (Some(&["("]), None))?;
    let condition = compile_expression(tokenized)?;
    advance_check(tokenized, (Some(&[")"]), None))?;

    advance_check(tokenized, (Some(&["{"]), None))?;
    let while_body = compile_statements(tokenized)?;
    advance_check(tokenized, (Some(&["}"]), None))?;

    Ok(Statement::While {
        condition,
        while_body,
    })
}

fn compile_let(tokenized: &mut Tokens) -> StatementReturn {
    // letStatements => 'let' varName ?('[', expression, ']')? '=' expression ';'
    advance_check(tokenized, (Some(&["let"]), Some(&[TokenTypes::Keyword])))?;
    let var_name = advance_check(tokenized, (None, Some(&[TokenTypes::Identifier])))?.0;

    let mut optional_index = None;
    if tokenized.peek().unwrap() == ("[".to_owned(), TokenTypes::Symbol) {
        advance_check(tokenized, (Some(&["["]), None))?;
        optional_index = Some(compile_expression(tokenized)?);
        advance_check(tokenized, (Some(&["]"]), None))?;
    }

    advance_check(tokenized, (Some(&["="]), None))?;
    let expression = compile_expression(tokenized)?;
    advance_check(tokenized, (Some(&[";"]), None))?;

    Ok(Statement::Let {
        var_name,
        optional_index,
        expression,
    })
}

fn compile_do(tokenized: &mut Tokens) -> StatementReturn {
    // do subroutineCall
    // do: 'name' '(' expressionlist ')' || 'Class/Var name' '.' '(' expressionlist ')'

    advance_check(tokenized, (Some(&["do"]), Some(&[TokenTypes::Keyword])))?;
    // name
    let mut parent_name = None;
    let mut subroutine_name = advance_check(tokenized, (None, Some(&[TokenTypes::Identifier])))?.0;

    // ( or .
    if &tokenized.peek().unwrap().0[..] == "." {
        parent_name = Some(subroutine_name);
        advance_check(tokenized, (Some(&["."]), None))?;
        subroutine_name = advance_check(tokenized, (None, Some(&[TokenTypes::Identifier])))?.0;
    }

    advance_check(tokenized, (Some(&["("]), None))?;
    let parameters = compile_expression_list(tokenized)?;
    advance_check(tokenized, (Some(&[")"]), None))?;
    advance_check(tokenized, (Some(&[";"]), None))?;

    Ok(Statement::Do(CallSubroutine {
        parent_name,
        subroutine_name,
        parameters,
    }))
}

fn compile_return(tokenized: &mut Tokens) -> StatementReturn {
    // return
    advance_check(tokenized, (Some(&["return"]), None))?;

    let mut return_expressions = None;
    // ; or identifier; or identifier, identifier .. ;
    if ";" != &tokenized.peek().unwrap().0[..] {
        return_expressions = Some(compile_expression(tokenized)?);
    }

    advance_check(tokenized, (Some(&[";"]), None))?;

    Ok(Statement::Return(return_expressions))
}

fn compile_expression(tokenized: &mut Tokens) -> Result<Expression, CompileError> {
    let mut expression = compile_term(tokenized)?;

    while let Some(tok) = tokenized.peek() {
        if is_binary_operator(&tok.0[..]) {
            let binary_op = Op::string_to_binary_op(
                advance_check(tokenized, (None, Some(&[TokenTypes::Symbol])))?.0,
            );
            expression.push(binary_op);
            expression.extend(compile_term(tokenized)?);
        } else {
            break;
        }
    }

    Ok(Expression(expression))
}

fn compile_expression_list(tokenized: &mut Tokens) -> Result<Vec<Expression>, CompileError> {
    let mut expressions: Vec<Expression> = Vec::new();

    if tokenized.peek().unwrap().0 != ")" {
        expressions.push(compile_expression(tokenized)?);
    }
    while let Some((token, _token_type)) = tokenized.peek() {
        if token == ")" {
            break;
        }
        advance_check(tokenized, (Some(&[","]), None))?;
        expressions.push(compile_expression(tokenized)?);
    }

    Ok(expressions)
}

fn compile_term(tokenized: &mut Tokens) -> Result<Vec<Terms>, CompileError> {
    // IntegerConstant | StringConstant | Keyword | varName |
    // varName '[' expression ']' | subroutine | '(' expression ')' | operationTerm
    let mut terms_output: Vec<Terms> = Vec::new();

    let (peek_token, peek_token_type) = tokenized.peek().unwrap();
    //println!("{:?}", (&peek_token, &peek_token_type));

    if peek_token_type == TokenTypes::Symbol {
        let current_symbol = &peek_token[..];
        match current_symbol {
            "-" | "~" => {
                /* UnaryOp Term (e.g. -1 ) */
                terms_output.push(Op::string_to_unary_op(
                    advance_check(tokenized, (None, None))?.0,
                ));
                terms_output.extend(compile_term(tokenized)?);
            }
            "(" => {
                // ( expression )
                advance_check(tokenized, (Some(&["("]), None))?;
                terms_output.push(Terms::Parenthesis(compile_expression(tokenized)?));
                advance_check(tokenized, (Some(&[")"]), None))?;
            }
            _ => {
                println!("Saw: {:?}", peek_token);
                return Err(CompileError::new("Unexpected symbol: ", tokenized));
            }
        }
    }
    // Else, identifier, keyword, integerconstant, stringconstant (i.e. Anything apart from symbol first)
    else {
        let (term_name, term_type) = advance_check(tokenized, (None, None))?;

        match term_type {
            TokenTypes::IntegerConstant => {
                terms_output.push(Terms::IntegerConstant(term_name.parse::<u16>().unwrap()))
            }
            TokenTypes::StringConstant => terms_output.push(Terms::StringConstant(term_name)),
            TokenTypes::Keyword => match &term_name[..] {
                "true" => terms_output.push(Terms::KeywordConstant(KeywordConstant::True)),
                "false" => terms_output.push(Terms::KeywordConstant(KeywordConstant::False)),
                "null" => terms_output.push(Terms::KeywordConstant(KeywordConstant::Null)),
                "this" => terms_output.push(Terms::KeywordConstant(KeywordConstant::This)),
                _ => {
                    return Err(CompileError::new("Wrong keyword given", tokenized));
                }
            },
            TokenTypes::Identifier => {
                // Need to navigate by looking ahead
                let peek_ahead_tuple = tokenized.peek().unwrap();

                if peek_ahead_tuple.1 == TokenTypes::Symbol {
                    match &peek_ahead_tuple.0[..] {
                        "[" => {
                            // varName '[' expression ']'
                            advance_check(tokenized, (Some(&["["]), None))?;
                            terms_output
                                .push(Terms::Bracket(term_name, compile_expression(tokenized)?));
                            advance_check(tokenized, (Some(&["]"]), None))?;
                        }
                        "." => {
                            // Syntax: subroutine.name
                            advance_check(tokenized, (Some(&["."]), None))?;
                            // Identifier
                            let subroutine_name =
                                advance_check(tokenized, (None, Some(&[TokenTypes::Identifier])))?
                                    .0;

                            // ( expression list )
                            advance_check(tokenized, (Some(&["("]), None))?;

                            let parameters = compile_expression_list(tokenized)?;

                            advance_check(tokenized, (Some(&[")"]), None))?;

                            terms_output.push(Terms::CallSubroutine(CallSubroutine {
                                parent_name: Some(term_name),
                                subroutine_name,
                                parameters,
                            }));
                        }
                        "(" => {
                            // ( expression )
                            advance_check(tokenized, (Some(&["("]), None))?;
                            terms_output.push(Terms::Parenthesis(compile_expression(tokenized)?));
                            advance_check(tokenized, (Some(&[")"]), None))?;
                        }
                        ")" | "]" | "," | ";" => {
                            //println!("Not sure if this is valid -- look closely at closure after identifiers");
                            terms_output.push(Terms::VarName(term_name))
                        }
                        _ => {
                            if is_binary_operator(&peek_ahead_tuple.0[..]) {
                                /* operationTerm, + ... */
                                terms_output.push(Terms::VarName(term_name));
                                terms_output.push(Op::string_to_binary_op(
                                    advance_check(tokenized, (None, None))?.0,
                                ));
                                terms_output.extend(compile_term(tokenized)?);
                            } else {
                                return Err(CompileError::new("Invalid symbol given", tokenized));
                            }
                        }
                    }
                } else {
                    terms_output.push(Terms::VarName(term_name));
                }
            }
            TokenTypes::Symbol => {
                return Err(CompileError::new(
                    "Should've been in if statement",
                    tokenized,
                ))
            }
        };
    }
    Ok(terms_output)
}

fn compile_var_dec(tokenized: &mut Tokens) -> VarDeclaration {
    // Compiles: var type name ... ;
    let mut var_declar: Vec<DeclareVariable> = Vec::new();
    advance_check(tokenized, (Some(&["var"]), Some(&[TokenTypes::Keyword])))?;

    // var Type
    let name_type = VarTypes::string_to_vartype(
        advance_check(
            tokenized,
            (None, Some(&[TokenTypes::Keyword, TokenTypes::Identifier])),
        )?
        .0,
    );

    // var_name -> ", moreNames.. ;", ";"
    let names = compile_name_list(tokenized)?;

    // end of statement
    advance_check(tokenized, (Some(&[";"]), None))?;

    for name in names {
        var_declar.push(DeclareVariable {
            name,
            name_type: name_type.clone(),
            kind: VarKinds::Local,
        })
    }

    Ok(var_declar)
}

fn compile_parameter_list(tokenized: &mut Tokens) -> VarDeclaration {
    // check if empty ->
    let mut parameters_declar: Vec<DeclareVariable> = Vec::new();

    if tokenized.peek().unwrap().0 == ")" {
        return Ok(parameters_declar);
    }

    loop {
        let name_type = VarTypes::string_to_vartype(
            advance_check(
                tokenized,
                (None, Some(&[TokenTypes::Keyword, TokenTypes::Identifier])),
            )?
            .0,
        );
        let name = advance_check(tokenized, (None, Some(&[TokenTypes::Identifier])))?.0;

        parameters_declar.push(DeclareVariable {
            name,
            name_type,
            kind: VarKinds::Argument,
        });

        match &tokenized.peek().unwrap().0[..] {
            "," => {
                advance_check(tokenized, (Some(&[","]), None))?;
            }
            ")" => break,
            _ => return Err(CompileError::new("@namelist", tokenized)),
        }
    }

    Ok(parameters_declar)
}

fn compile_name_list(tokenized: &mut Tokens) -> Result<Vec<String>, CompileError> {
    let mut names: Vec<String> = Vec::new();

    names.push(advance_check(tokenized, (None, Some(&[TokenTypes::Identifier])))?.0);
    // loop
    loop {
        match &tokenized.peek().unwrap().0[..] {
            ";" => {
                break;
            }
            "," => {
                advance_check(tokenized, (Some(&[","]), None))?;
                names.push(advance_check(tokenized, (None, Some(&[TokenTypes::Identifier])))?.0);
            }
            _ => return Err(CompileError::new("@namelist", tokenized)),
        }
    }

    Ok(names)
}

fn is_binary_operator(token: &str) -> bool {
    match token {
        "+" | "-" | "/" | "*" | "=" | "&" | "|" | "<" | ">" | "~" => true,
        _ => false,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn basic_class_example() {
        use super::TokenTypes::*;
        let mut tokenized = Tokens {
            token_stream: vec![
                ("class".to_owned(), Keyword),
                ("Square".to_owned(), Identifier),
                ("{".to_owned(), Symbol),
                ("static".to_owned(), Keyword),
                ("int".to_owned(), Keyword),
                ("st".to_owned(), Identifier),
                (";".to_owned(), Symbol),
                ("field".to_owned(), Keyword),
                ("boolean".to_owned(), Keyword),
                ("n1".to_owned(), Identifier),
                (",".to_owned(), Symbol),
                ("n2".to_owned(), Identifier),
                (";".to_owned(), Symbol),
                // class body: constructor / methods / functions
                ("constructor".to_owned(), Keyword),
                ("parent_class_name".to_owned(), Identifier),
                ("new".to_owned(), Identifier),
                ("(".to_owned(), Symbol),
                // Argument list
                ("int".to_owned(), Keyword),
                ("arg1".to_owned(), Identifier),
                (",".to_owned(), Symbol),
                ("int".to_owned(), Keyword),
                ("arg2".to_owned(), Identifier),
                (")".to_owned(), Symbol),
                ("{".to_owned(), Symbol),
                ("}".to_owned(), Symbol),
                ("method".to_owned(), Keyword),
                ("int".to_owned(), Keyword),
                ("m".to_owned(), Identifier),
                // remember method includes "this" as an argument
                ("(".to_owned(), Symbol),
                (")".to_owned(), Symbol),
                ("{".to_owned(), Symbol),
                ("}".to_owned(), Symbol),
                ("function".to_owned(), Keyword),
                ("void".to_owned(), Keyword),
                ("f".to_owned(), Identifier),
                ("(".to_owned(), Symbol),
                (")".to_owned(), Symbol),
                ("{".to_owned(), Symbol),
                // local variables
                ("var".to_owned(), Keyword),
                ("int".to_owned(), Keyword),
                ("i".to_owned(), Identifier),
                (",".to_owned(), Symbol),
                ("j".to_owned(), Identifier),
                (";".to_owned(), Symbol),
                // local var with another class as the type
                ("var".to_owned(), Keyword),
                ("OtherClass".to_owned(), Keyword),
                ("c".to_owned(), Identifier),
                (";".to_owned(), Symbol),
                // let
                ("let".to_owned(), Keyword),
                ("st".to_owned(), Identifier),
                ("=".to_owned(), Symbol),
                ("-".to_owned(), Symbol),
                ("1".to_owned(), IntegerConstant),
                (";".to_owned(), Symbol),
                // subroutine call
                ("do".to_owned(), Keyword),
                ("f".to_owned(), Identifier),
                ("(".to_owned(), Symbol),
                ("w".to_owned(), Identifier),
                (",".to_owned(), Symbol),
                ("1".to_owned(), IntegerConstant),
                (")".to_owned(), Symbol),
                (";".to_owned(), Symbol),
                ("}".to_owned(), Symbol),
                ("}".to_owned(), Symbol),
            ],
        };
        // println!("Parsed_block: {:#?}", compile_class(&mut tokenized).unwrap());

        assert!(compile_class(&mut tokenized).is_ok());
    }

    #[test]
    fn test_nested_compile_expression() {
        let mut int_const: Tokens = Tokens {
            token_stream: vec![
                ("1".to_owned(), TokenTypes::IntegerConstant),
                ("/".to_owned(), TokenTypes::Symbol),
                ("(".to_owned(), TokenTypes::Symbol),
                ("(".to_owned(), TokenTypes::Symbol),
                ("1".to_owned(), TokenTypes::IntegerConstant),
                (")".to_owned(), TokenTypes::Symbol),
                ("+".to_owned(), TokenTypes::Symbol),
                ("1".to_owned(), TokenTypes::IntegerConstant),
                (")".to_owned(), TokenTypes::Symbol),
            ],
        };
        assert_eq!(
            compile_expression(&mut int_const).unwrap(),
            Expression(vec![
                Terms::IntegerConstant(1),
                Terms::Op(Op::Divide),
                Terms::Parenthesis(Expression(vec![
                    Terms::Parenthesis(Expression(vec![Terms::IntegerConstant(1),])),
                    Terms::Op(Op::Plus),
                    Terms::IntegerConstant(1),
                ]))
            ])
        );
    }
    #[test]
    fn test_compile_expression_list() {
        let mut int_const: Tokens = Tokens {
            token_stream: vec![
                ("1".to_owned(), TokenTypes::IntegerConstant),
                (",".to_owned(), TokenTypes::Symbol),
                ("hi".to_owned(), TokenTypes::StringConstant),
            ],
        };
        assert_eq!(
            compile_expression_list(&mut int_const).unwrap(),
            vec![
                Expression(vec![Terms::IntegerConstant(1),]),
                Expression(vec![Terms::StringConstant("hi".to_owned()),])
            ]
        );
    }

    #[test]
    fn test_compile_term() {
        // IntegerConstant | StringConstant | Keyword | varName |
        // varName '[' expression ']' | subroutine | '(' expression ')' | operationTerm

        let mut call_subroutine_test: Tokens = Tokens {
            token_stream: vec![
                ("Arr".to_owned(), TokenTypes::Identifier),
                (".".to_owned(), TokenTypes::Symbol),
                ("new".to_owned(), TokenTypes::Identifier),
                ("(".to_owned(), TokenTypes::Symbol),
                ("length".to_owned(), TokenTypes::Identifier),
                (")".to_owned(), TokenTypes::Symbol),
            ],
        };
        assert_eq!(
            compile_term(&mut call_subroutine_test).unwrap(),
            vec![Terms::CallSubroutine(CallSubroutine {
                parent_name: Some("Arr".to_owned()),
                subroutine_name: "new".to_owned(),
                parameters: vec![Expression(vec![Terms::VarName("length".to_owned())])],
            })]
        );

        let mut parenthesis_test: Tokens = Tokens {
            token_stream: vec![
                ("(".to_owned(), TokenTypes::Symbol),
                ("1".to_owned(), TokenTypes::IntegerConstant),
                (")".to_owned(), TokenTypes::Symbol),
            ],
        };
        assert_eq!(
            compile_term(&mut parenthesis_test).unwrap(),
            vec![Terms::Parenthesis(Expression(vec![
                Terms::IntegerConstant(1),
            ]))]
        );

        let mut bracket_test: Tokens = Tokens {
            token_stream: vec![
                ("Arr".to_owned(), TokenTypes::Identifier),
                ("[".to_owned(), TokenTypes::Symbol),
                ("1".to_owned(), TokenTypes::IntegerConstant),
                ("]".to_owned(), TokenTypes::Symbol),
            ],
        };
        assert_eq!(
            compile_term(&mut bracket_test).unwrap(),
            vec![Terms::Bracket(
                "Arr".to_owned(),
                Expression(vec![Terms::IntegerConstant(1),])
            )]
        );

        let mut unary_op_test: Tokens = Tokens {
            token_stream: vec![
                ("-".to_owned(), TokenTypes::Symbol),
                ("1".to_owned(), TokenTypes::IntegerConstant),
            ],
        };
        assert_eq!(
            compile_term(&mut unary_op_test).unwrap(),
            vec![Terms::Op(Op::Neg), Terms::IntegerConstant(1),]
        );

        let mut var_name_test: Tokens = Tokens {
            token_stream: vec![
                ("hello".to_owned(), TokenTypes::Identifier),
                ("+".to_owned(), TokenTypes::Symbol),
                ("1".to_owned(), TokenTypes::IntegerConstant),
            ],
        };
        assert_eq!(
            compile_term(&mut var_name_test).unwrap(),
            vec![
                Terms::VarName("hello".to_owned()),
                Terms::Op(Op::Plus),
                Terms::IntegerConstant(1),
            ]
        );

        let mut str_const: Tokens = Tokens {
            token_stream: vec![("test".to_owned(), TokenTypes::StringConstant)],
        };
        assert_eq!(
            compile_term(&mut str_const).unwrap(),
            vec![Terms::StringConstant("test".to_owned())]
        );

        let mut keyw_const: Tokens = Tokens {
            token_stream: vec![("this".to_owned(), TokenTypes::Keyword)],
        };
        assert_eq!(
            compile_term(&mut keyw_const).unwrap(),
            vec![Terms::KeywordConstant(KeywordConstant::This)]
        );
    }

    #[test]
    fn test_compile_var_dec() {
        // never called when expected to be empty -- Var declaration && subroutine names
        let mut tokenized: Tokens = Tokens {
            token_stream: vec![
                ("var".to_owned(), TokenTypes::Keyword),
                ("int".to_owned(), TokenTypes::Keyword),
                ("i".to_owned(), TokenTypes::Identifier),
                (",".to_owned(), TokenTypes::Symbol),
                ("j".to_owned(), TokenTypes::Identifier),
                (";".to_owned(), TokenTypes::Symbol),
            ],
        };
        assert_eq!(
            compile_var_dec(&mut tokenized).unwrap(),
            vec![
                DeclareVariable {
                    name: "i".to_owned(),
                    name_type: VarTypes::Int,
                    kind: VarKinds::Local
                },
                DeclareVariable {
                    name: "j".to_owned(),
                    name_type: VarTypes::Int,
                    kind: VarKinds::Local
                },
            ]
        );
    }
    #[test]
    fn test_compile_parameter_list() {
        let mut empty: Tokens = Tokens {
            token_stream: vec![(")".to_owned(), TokenTypes::Symbol)],
        };
        assert_eq!(compile_parameter_list(&mut empty).unwrap(), vec![]);

        let mut tokenized: Tokens = Tokens {
            token_stream: vec![
                ("int".to_owned(), TokenTypes::Keyword),
                ("i".to_owned(), TokenTypes::Identifier),
                (",".to_owned(), TokenTypes::Symbol),
                ("boolean".to_owned(), TokenTypes::Keyword),
                ("j".to_owned(), TokenTypes::Identifier),
                (")".to_owned(), TokenTypes::Symbol),
            ],
        };
        assert_eq!(
            compile_parameter_list(&mut tokenized).unwrap(),
            vec![
                DeclareVariable {
                    name: "i".to_owned(),
                    name_type: VarTypes::Int,
                    kind: VarKinds::Argument
                },
                DeclareVariable {
                    name: "j".to_owned(),
                    name_type: VarTypes::Boolean,
                    kind: VarKinds::Argument
                },
            ]
        );
    }
    #[test]
    fn test_compile_name_list() {
        // never called when expected to be empty -- Var declaration && subroutine names
        let mut tokenized: Tokens = Tokens {
            token_stream: vec![
                ("i".to_owned(), TokenTypes::Identifier),
                (",".to_owned(), TokenTypes::Symbol),
                ("j".to_owned(), TokenTypes::Identifier),
                (";".to_owned(), TokenTypes::Symbol),
            ],
        };
        assert_eq!(
            compile_name_list(&mut tokenized).unwrap(),
            vec!["i".to_owned(), "j".to_owned()]
        );
    }
    #[test]
    fn test_is_binary_operator() {
        assert_eq!(is_binary_operator("="), true);
        assert_eq!(is_binary_operator("!"), false);
    }

    #[test]
    fn compare_tokens_types() {
        assert!(TokenTypes::Identifier != TokenTypes::Keyword);
        assert!(TokenTypes::Keyword == TokenTypes::Keyword);
        assert!(&TokenTypes::Identifier == &TokenTypes::Identifier);
    }

    #[test]
    fn iterator() {
        use super::TokenTypes::*;
        let mut tokenized = Tokens {
            token_stream: vec![
                ("class".to_owned(), Keyword),
                ("Square".to_owned(), Identifier),
                ("{".to_owned(), Symbol),
                ("}".to_owned(), Symbol),
            ],
        };

        assert_eq!(tokenized.peek().unwrap(), ("class".to_owned(), Keyword));
        assert_eq!(tokenized.next().unwrap(), ("class".to_owned(), Keyword));
        assert_eq!(tokenized.peek().unwrap(), ("Square".to_owned(), Identifier));
        assert_eq!(tokenized.next().unwrap(), ("Square".to_owned(), Identifier));
        assert_eq!(tokenized.next().unwrap(), ("{".to_owned(), Symbol));
        assert_eq!(tokenized.next().unwrap(), ("}".to_owned(), Symbol));
    }
}
