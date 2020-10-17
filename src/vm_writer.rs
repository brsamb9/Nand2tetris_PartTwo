// VM emits VM commands
use super::compilation_engine::*;
use super::symbol_table::*;
use std::fmt;

#[derive(Debug)]
pub struct VmInstruError {
    pub error: String,
}
impl VmInstruError {
    fn new(err: String) -> Self {
        VmInstruError {
            error: format!("Err: {}", err),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum VmInstruction {
    Push(VmSegment, usize), // Virtual memory segment, index
    Pop(VmSegment, usize),
    //Arithmetic,
    Add, // +
    Sub, // -
    Neg, // !
    Eq,  // =
    Gt,  // >
    Lt,  // <
    And, // &
    Or,  // |
    Not, // ~
    //
    Label(String),
    GoTo(String),
    If(String),
    Call(String, usize),
    Function(String, usize),
    Return,
}

impl fmt::Display for VmInstruction {
    fn fmt(&self, vm_file: &mut fmt::Formatter) -> fmt::Result {
        use self::VmInstruction::*;
        match self {
            Push(seg, index) => write!(vm_file, "push {} {}", seg, index),
            Pop(seg, index) => write!(vm_file, "pop {} {}", seg, index),
            Add => write!(vm_file, "add"),
            Sub => write!(vm_file, "sub"),
            Eq => write!(vm_file, "eq"),
            Gt => write!(vm_file, "gt"),
            Lt => write!(vm_file, "lt"),
            And => write!(vm_file, "and"),
            Or => write!(vm_file, "or"),
            Neg => write!(vm_file, "neg"),
            Not => write!(vm_file, "not"),
            Label(s) => write!(vm_file, "label {}", s),
            GoTo(s) => write!(vm_file, "goto {}", s),
            If(s) => write!(vm_file, "if-goto {}", s),
            Call(function, n_args) => write!(vm_file, "call {} {}", function, n_args),
            Function(function, n_vars) => write!(vm_file, "function {} {}", function, n_vars),
            Return => write!(vm_file, "return"),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum VmSegment {
    Constant,
    Argument,
    Local,
    Static,
    This,
    That,
    Pointer,
    Temp,
}
impl From<VarKinds> for VmSegment {
    fn from(convert: VarKinds) -> Self {
        match convert {
            VarKinds::Static => VmSegment::Static,
            VarKinds::Field => VmSegment::This,
            VarKinds::Local => VmSegment::Local,
            VarKinds::Argument => VmSegment::Argument,
        }
    }
}
impl fmt::Display for VmSegment {
    fn fmt(&self, vm_file: &mut fmt::Formatter) -> fmt::Result {
        use self::VmSegment::*;
        match *self {
            Constant => write!(vm_file, "constant"),
            Argument => write!(vm_file, "argument"),
            Local => write!(vm_file, "local"),
            Static => write!(vm_file, "static"),
            This => write!(vm_file, "this"),
            That => write!(vm_file, "that"),
            Pointer => write!(vm_file, "pointer"),
            Temp => write!(vm_file, "temp"),
        }
    }
}
// for unique labels
static mut COUNT: usize = 0;

fn incr_count() -> usize {
    unsafe {
        COUNT += 1;
        COUNT
    }
}

type VmWriter = Result<Vec<VmInstruction>, VmInstruError>;

pub fn write_vm_code(compiled: &Parsed<ProgramBlock>) -> VmWriter {
    // Init
    let mut vm_instructions: Vec<VmInstruction> = Vec::new();

    // Class-level
    let class_level_data = &compiled.data;
    let mut sym_table = ClassAndSubroutineTables::new(&class_level_data.name[..]);
    SymbolTable::add_vars_to_table(&mut sym_table.0, "class", &class_level_data.declar_var)
        .unwrap();

    // Subroutine-level
    let num_of_subroutines = Parsed::number_of_subroutines(&compiled);
    for i in 0..num_of_subroutines {
        ClassAndSubroutineTables::start_subroutine(&mut sym_table);
        let subroutine_level_data = &compiled.nested[i].data;
        SymbolTable::add_vars_to_table(
            &mut sym_table.1,
            "subroutine",
            &subroutine_level_data.declar_var,
        )
        .unwrap();

        // Write vm code for each subroutine
        let full_name = format!(
            "{}.{}",
            class_level_data.name.clone(),
            subroutine_level_data.name
        );
        let block = subroutine_level_data.subroutine_type.clone();
        let n_local = ClassAndSubroutineTables::var_count(&sym_table, VarKinds::Local);

        vm_instructions.push(VmInstruction::Function(full_name, n_local));

        if block == Block::Method {
            // Push 'this'
            vm_instructions.push(VmInstruction::Push(VmSegment::Argument, 0));
            vm_instructions.push(VmInstruction::Pop(VmSegment::Pointer, 0));
        } else if block == Block::Constructor {
            let n_field = ClassAndSubroutineTables::var_count(&sym_table, VarKinds::Field);
            // Fields for constructor
            vm_instructions.push(VmInstruction::Push(VmSegment::Constant, n_field));
            vm_instructions.push(VmInstruction::Call("Memory.alloc".to_owned(), 1));
            vm_instructions.push(VmInstruction::Pop(VmSegment::Pointer, 0));
        }

        for statement in &subroutine_level_data.body_statements {
            vm_instructions.extend(write_vm_statement(&statement, &sym_table)?);
        }
    }

    Ok(vm_instructions)
}

fn write_vm_statements(statements: &Vec<Statement>, table: &ClassAndSubroutineTables) -> VmWriter {
    let mut vm_statements: Vec<VmInstruction> = Vec::new();

    for statement in statements {
        vm_statements.extend(write_vm_statement(statement, table)?);
    }

    Ok(vm_statements)
}

fn write_vm_statement(statement: &Statement, table: &ClassAndSubroutineTables) -> VmWriter {
    match statement {
        Statement::Let {
            var_name,
            optional_index,
            expression,
        } => write_vm_let(var_name, optional_index, expression, &table),
        Statement::If {
            condition,
            if_body,
            else_body,
        } => write_vm_if(condition, if_body, else_body, &table),
        Statement::While {
            condition,
            while_body,
        } => write_vm_while(condition, while_body, &table),
        Statement::Do(CallSubroutine {
            parent_name,
            subroutine_name,
            parameters,
        }) => write_vm_do(parent_name, subroutine_name, parameters, &table),
        Statement::Return(ref expression) => write_vm_return(expression, &table),
    }
}

fn write_vm_let(
    var_name: &String,
    optional_index: &Option<Expression>,
    expression: &Expression,
    table: &ClassAndSubroutineTables,
) -> VmWriter {
    let mut vm_let: Vec<VmInstruction> = Vec::new();
    // Grab info from symbol table
    let (var, indx) = table.get(&var_name[..]).ok_or(VmInstruError::new(format!(
        "{} Not in symbol table (let)",
        var_name
    )))?;
    // Right hand side expression to vm instructions - used to extend vector later
    let rhs = write_vm_expression(expression, table)?;

    if let Some(index_expression) = optional_index {
        // e.g. hi[(1+2)] = ...
        vm_let.extend(write_vm_expression(index_expression, table)?);

        vm_let.push(VmInstruction::Push(VmSegment::from(var.kind), indx.clone()));
        vm_let.push(VmInstruction::Add); // to grab the right address (e.g. local base address + i)

        vm_let.extend(rhs);
        vm_let.push(VmInstruction::Pop(VmSegment::Temp, 0)); // save rhs to temporary

        vm_let.push(VmInstruction::Pop(VmSegment::Pointer, 1)); // Save previous address to pointer (That)
        vm_let.push(VmInstruction::Push(VmSegment::Temp, 0)); // put rhs back on stack and pop to That
        vm_let.push(VmInstruction::Pop(VmSegment::That, 0));
    } else {
        // hi = ...
        vm_let.extend(rhs);
        vm_let.push(VmInstruction::Pop(VmSegment::from(var.kind), indx.clone()));
    }

    Ok(vm_let)
}

fn write_vm_if(
    condition: &Expression,
    if_body: &Vec<Statement>,
    else_body: &Option<Vec<Statement>>,
    table: &ClassAndSubroutineTables,
) -> VmWriter {
    // labels
    let if_i = &incr_count().to_string()[..];
    let if_true = format!("IF_TRUE_{}", if_i);
    let if_false = format!("IF_FALSE_{}", if_i);
    let if_end = format!("IF_END_{}", if_i);

    // if (condition)
    let mut vm_if = write_vm_expression(condition, table)?;

    vm_if.push(VmInstruction::If(if_true.clone()));
    vm_if.push(VmInstruction::GoTo(if_false.clone()));
    vm_if.push(VmInstruction::Label(if_true));

    // if body -> jump over else
    vm_if.extend(write_vm_statements(if_body, table)?);
    vm_if.push(VmInstruction::GoTo(if_end.clone()));

    // else body
    vm_if.push(VmInstruction::Label(if_false));
    if let Some(else_vm) = else_body {
        vm_if.extend(write_vm_statements(else_vm, table)?);
    }

    vm_if.push(VmInstruction::Label(if_end));

    Ok(vm_if)
}

fn write_vm_while(
    condition: &Expression,
    while_body: &Vec<Statement>,
    table: &ClassAndSubroutineTables,
) -> VmWriter {
    // labels
    let while_i = &incr_count().to_string()[..];
    let while_true = format!("WHILE_{}", while_i);
    let while_end = format!("WHILE_END_{}", while_i);

    let mut vm_while: Vec<VmInstruction> = Vec::new();

    vm_while.push(VmInstruction::Label(while_true.clone()));
    vm_while.extend(write_vm_expression(condition, table)?);
    // if condition is not met -> Not flips to true and jumps to end label
    vm_while.push(VmInstruction::Not);
    vm_while.push(VmInstruction::If(while_end.clone()));
    // else
    vm_while.extend(write_vm_statements(while_body, table)?);
    vm_while.push(VmInstruction::GoTo(while_true));
    // jump to when condition is not met
    vm_while.push(VmInstruction::Label(while_end));

    Ok(vm_while)
}

fn write_vm_do(
    parent_name: &Option<String>,
    subroutine_name: &String,
    parameters: &Vec<Expression>,
    table: &ClassAndSubroutineTables,
) -> VmWriter {
    let mut vm_do: Vec<VmInstruction> = Vec::new();

    vm_do.extend(write_vm_subroutine_call(
        parent_name,
        subroutine_name,
        parameters,
        table,
    )?);
    vm_do.push(VmInstruction::Pop(VmSegment::Temp, 0));

    Ok(vm_do)
}

fn write_vm_return(expression: &Option<Expression>, table: &ClassAndSubroutineTables) -> VmWriter {
    let mut vm_return: Vec<VmInstruction> = Vec::new();

    if let Some(return_expression) = expression {
        vm_return.extend(write_vm_expression(return_expression, table)?);
    } else {
        // void return - need to return an arbituary value regardless (and pop off later)
        vm_return.push(VmInstruction::Push(VmSegment::Constant, 0));
    }
    vm_return.push(VmInstruction::Return);

    Ok(vm_return)
}

fn write_vm_expression(expression: &Expression, table: &ClassAndSubroutineTables) -> VmWriter {
    // empty
    if expression.0.len() == 0 {
        return Ok(vec![]);
    }

    let mut vm_expression: Vec<VmInstruction> = Vec::new();
    // iterator due to postfix (e.g. 1 + 1 -> 1 1 + [needs to 'stall' operator])
    let mut expr_iter = expression.clone().into_iter();

    while let Some(next_term) = expr_iter.next() {
        match next_term {
            Terms::Op(ref op) => {
                match op {
                    // binary - Postfix of a + b -> a b +
                    Op::Plus
                    | Op::Minus
                    | Op::Divide
                    | Op::Multiply
                    | Op::Eq
                    | Op::And
                    | Op::Or
                    | Op::Lt
                    | Op::Gt => {
                        vm_expression.extend(write_vm_term(&expr_iter.next().unwrap(), table)?);
                        vm_expression.push(VmInstruction::from(op));
                    }
                    // Unary - Postfix of -a -> a
                    Op::Neg | Op::Not => {
                        vm_expression.extend(write_vm_term(&expr_iter.next().unwrap(), table)?);
                        vm_expression.push(VmInstruction::from(op));
                    }
                }
            }
            _ => vm_expression.extend(write_vm_term(&next_term, table)?),
        }
    }
    Ok(vm_expression)
}

fn write_vm_term(term: &Terms, table: &ClassAndSubroutineTables) -> VmWriter {
    match term {
        Terms::IntegerConstant(i) => Ok(vec![VmInstruction::Push(
            VmSegment::Constant,
            i.clone() as usize,
        )]),
        Terms::StringConstant(s) => write_vm_string_constant(&s[..]),
        Terms::KeywordConstant(k) => Ok(match k {
            KeywordConstant::True => vec![
                VmInstruction::Push(VmSegment::Constant, 1),
                VmInstruction::Not,
            ],
            KeywordConstant::False | KeywordConstant::Null => {
                vec![VmInstruction::Push(VmSegment::Constant, 0)]
            }
            KeywordConstant::This => vec![VmInstruction::Push(VmSegment::Pointer, 0)],
        }),
        Terms::VarName(ref name) => {
            let (var, idx) = table.get(name).ok_or(VmInstruError::new(format!(
                "{} Not in symbol table (varName)",
                name
            )))?;

            Ok(vec![VmInstruction::Push(
                VmSegment::from(var.kind),
                idx.clone(),
            )])
        }
        Terms::Bracket(ref name, ref expres) => {
            // a[i] / a[i+1]
            let mut vm_index = write_vm_expression(expres, table)?;
            let (var, indx) = table.get(&name[..]).ok_or(VmInstruError::new(format!(
                "{} Not in symbol table (bracket)",
                name
            )))?;

            vm_index.push(VmInstruction::Push(VmSegment::from(var.kind), indx.clone()));
            vm_index.push(VmInstruction::Add);

            vm_index.push(VmInstruction::Pop(VmSegment::Pointer, 1));
            vm_index.push(VmInstruction::Push(VmSegment::That, 0));

            Ok(vm_index)
        }
        Terms::Parenthesis(ref expres) => Ok(write_vm_expression(expres, table)?),
        Terms::CallSubroutine(ref call_subroutine) => Ok(write_vm_subroutine_call(
            &call_subroutine.parent_name,
            &call_subroutine.subroutine_name,
            &call_subroutine.parameters,
            table,
        )?),

        Terms::Op(ref op) => {
            return Err(VmInstruError::new(format!(
                "Operation in term was used -> {:?}, assumption was used in expression",
                op
            )))
        }
    }
}

fn write_vm_string_constant(string: &str) -> VmWriter {
    let mut vm_string: Vec<VmInstruction> = vec![];

    vm_string.push(VmInstruction::Push(VmSegment::Constant, string.len()));
    vm_string.push(VmInstruction::Call("String.new".to_owned(), 1));

    for char_byte in string.as_bytes() {
        vm_string.push(VmInstruction::Push(
            VmSegment::Constant,
            *char_byte as usize,
        ));
        vm_string.push(VmInstruction::Call("String.appendChar".to_owned(), 2));
    }

    Ok(vm_string)
}

fn write_vm_subroutine_call(
    parent_name: &Option<String>,
    subroutine_name: &String,
    parameters: &Vec<Expression>,
    table: &ClassAndSubroutineTables,
) -> VmWriter {
    // subroutine_name(expr) | class_name.subroutine_name(expr) | var_name.subroutine_name(expr)

    let mut n_args = parameters.len();
    // let var_type = &var.name_type;

    let function_name = match parent_name {
        Some(f) => {
            // https://stackoverflow.com/questions/38406793/why-is-capitalizing-the-first-letter-of-a-string-so-convoluted-in-rust
            let mut c = f.chars();
            let c_parent_name = match c.next() {
                None => String::new(),
                Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
            };
            format!("{}.{}", c_parent_name, subroutine_name)
        }

        None => {
            let class_name = table.2.clone();
            format!("{}.{}", class_name, subroutine_name)
        }
    };

    let mut vm_subroutine_call: Vec<VmInstruction> = Vec::new();
    // if class
    if let Some(parent) = parent_name {
        // Method call
        if let Some((obj_var, obj_indx)) = table.get(&parent[..]) {
            // push "this"
            vm_subroutine_call.push(VmInstruction::Push(
                VmSegment::from(obj_var.kind),
                obj_indx.clone(),
            ));
            n_args += 1;
        }
    } else {
        // Is method -> Push 'this'
        vm_subroutine_call.push(VmInstruction::Push(VmSegment::Pointer, 0));
        n_args += 1;
    }

    for para in parameters {
        vm_subroutine_call.extend(write_vm_expression(para, table)?);
    }

    vm_subroutine_call.push(VmInstruction::Call(function_name, n_args));

    Ok(vm_subroutine_call)
}

#[cfg(test)]
mod test {
    use super::*;

    // Focused on integration test with Nand2Tetris provided tools

    #[test]
    fn test_subroutine_call() {
        let mut table = ClassAndSubroutineTables::new("class_test");
        SymbolTable::add_vars_to_table(
            &mut table.1,
            "subroutine",
            &vec![DeclareVariable {
                name: "sub_call".to_owned(),
                name_type: VarTypes::Class("Class1".to_owned()),
                kind: VarKinds::Argument,
            }],
        )
        .unwrap();

        let a = CallSubroutine {
            parent_name: Some("Class1".to_owned()),
            subroutine_name: "sub_call".to_owned(),
            parameters: vec![],
        };
        println!(
            "{:?}",
            write_vm_subroutine_call(&a.parent_name, &a.subroutine_name, &a.parameters, &table)
                .unwrap()
        );
    }
    #[test]
    fn vm_term() {
        let mut table = ClassAndSubroutineTables::new("class_test");
        // VarName
        SymbolTable::add_to_table(
            &mut table.1,
            "subroutine",
            &DeclareVariable {
                name: "hi".to_owned(),
                name_type: VarTypes::String,
                kind: VarKinds::Argument,
            },
        )
        .unwrap();
        assert_eq!(
            write_vm_term(&Terms::VarName("hi".to_owned()), &table).unwrap(),
            vec![VmInstruction::Push(VmSegment::Argument, 0)]
        );
    }

    #[test]
    fn vm_arithmetic() {
        let table = ClassAndSubroutineTables::new("class_test");

        // binary operation
        assert_eq!(
            write_vm_expression(
                &Expression(vec![
                    Terms::IntegerConstant(1),
                    Terms::Op(Op::Multiply),
                    Terms::IntegerConstant(1),
                ]),
                &table
            )
            .unwrap(),
            vec![
                VmInstruction::Push(VmSegment::Constant, 1),
                VmInstruction::Push(VmSegment::Constant, 1),
                VmInstruction::Call("Math.multiply".to_owned(), 2),
            ]
        );
        // unary
        assert_eq!(
            write_vm_expression(
                &Expression(vec![Terms::Op(Op::Neg), Terms::IntegerConstant(1),]),
                &table
            )
            .unwrap(),
            vec![
                VmInstruction::Push(VmSegment::Constant, 1),
                VmInstruction::Neg,
            ]
        );
        // return void expression
        assert_eq!(
            write_vm_expression(&Expression(vec![]), &table).unwrap(),
            vec![]
        );
    }

    #[test]
    fn output_vm_instruction() {
        use VmInstruction::*;
        use VmSegment::*;
        assert!(format!("{}", Pop(Local, 20)) == "pop local 20");
        assert!(format!("{}", Push(Static, 10)) == "push static 10");
        assert!(format!("{}", Add) == "add");
        assert!(format!("{}", Sub) == "sub");
        assert!(format!("{}", Neg) == "neg");
        assert!(format!("{}", Eq) == "eq");
        assert!(format!("{}", Lt) == "lt");
        assert!(format!("{}", And) == "and");
        assert!(format!("{}", Or) == "or");
        assert!(format!("{}", Label("test".to_owned())) == "label test");
        assert!(format!("{}", GoTo("jmp1".to_owned())) == "goto jmp1");
        assert!(format!("{}", If("jmp2".to_owned())) == "if-goto jmp2");
        assert!(format!("{}", Call("func1".to_owned(), 2)) == "call func1 2");
        assert!(format!("{}", Function("func2".to_owned(), 3)) == "function func2 3");
        assert!(format!("{}", Return) == "return");
    }
}
