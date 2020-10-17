// Creates symbol table at class-level and subroutine-level.
// name type kind Index
use super::compilation_engine::{DeclareVariable, VarKinds, VarTypes};
use std::collections::HashMap;
#[derive(Debug)]
pub struct SymbolTableError {
    pub error: String,
}
impl SymbolTableError {
    fn new(err: String) -> Self {
        SymbolTableError {
            error: format!("Err: {}", err),
        }
    }
}

#[derive(Debug)]
pub struct ClassAndSubroutineTables(pub SymbolTable, pub SymbolTable, pub String);

impl ClassAndSubroutineTables {
    pub fn new(class_name: &str) -> Self {
        // Class (0) and subroutine (1) table

        // https://stackoverflow.com/questions/38406793/why-is-capitalizing-the-first-letter-of-a-string-so-convoluted-in-rust
        let mut c = class_name.chars();
        let c_class_name = match c.next() {
            None => String::new(),
            Some(f) => f.to_uppercase().collect::<String>() + c.as_str(),
        };
        ClassAndSubroutineTables(SymbolTable::new(), SymbolTable::new(), c_class_name)
    }
    pub fn get(&self, name: &str) -> Option<&(DeclareVariable, usize)> {
        // Subroutine table first, then class
        self.1.table.get(name).or(self.0.table.get(name))
    }

    pub fn start_subroutine(&mut self) {
        self.1 = SymbolTable::new();
    }

    pub fn var_count(&self, kind: VarKinds) -> usize {
        match kind {
            // in class table (.0)
            VarKinds::Static => self.0.static_count,
            VarKinds::Field => self.0.field_count,
            // in subroutine table (.1)
            VarKinds::Argument => self.1.argument_count,
            VarKinds::Local => self.1.local_count,
        }
    }
    // pub fn kind_of(&self, name: &str) -> Option<VarKinds> {
    //     // return kind of given DeclareVariable
    //     Some(ClassAndSubroutineTables::get(&self, name)?.0.kind)
    // }
    // pub fn type_of(&self, name: &str) -> Option<VarTypes> {
    //     // return name_type of given DeclareVariable
    //     Some(
    //         ClassAndSubroutineTables::get(&self, name)?
    //             .0
    //             .name_type
    //             .clone(),
    //     )
    // }
    // pub fn index_of(&self, name: &str) -> Option<usize> {
    //     // return index of given DeclareVariable
    //     Some(ClassAndSubroutineTables::get(&self, name)?.1)
    // }
}

#[derive(Debug, PartialEq)]
pub struct SymbolTable {
    table: HashMap<String, (DeclareVariable, usize)>,
    static_count: usize,
    field_count: usize,
    local_count: usize,
    argument_count: usize,
}

impl SymbolTable {
    fn new() -> SymbolTable {
        SymbolTable {
            table: HashMap::new(),
            static_count: 0,
            field_count: 0,
            local_count: 0,
            argument_count: 0,
        }
    }
    pub fn add_to_table(
        &mut self,
        table_check: &str,
        var: &DeclareVariable,
    ) -> Result<(), SymbolTableError> {
        match var.kind {
            VarKinds::Field => {
                if table_check != "class" {
                    return Err(SymbolTableError::new(format!(
                        "Wrong table for field: {:?}",
                        var
                    )));
                }
                self.table
                    .insert(var.name.clone(), (var.clone(), self.field_count));
                self.field_count += 1;
            }
            VarKinds::Static => {
                if table_check != "class" {
                    return Err(SymbolTableError::new(format!(
                        "Wrong table for static: {:?}",
                        var
                    )));
                }
                self.table
                    .insert(var.name.clone(), (var.clone(), self.static_count));
                self.static_count += 1;
            }
            VarKinds::Local => {
                if table_check != "subroutine" {
                    return Err(SymbolTableError::new(format!(
                        "Wrong table for local: {:?}",
                        var
                    )));
                }
                self.table
                    .insert(var.name.clone(), (var.clone(), self.local_count));
                self.local_count += 1;
            }
            VarKinds::Argument => {
                if table_check != "subroutine" {
                    return Err(SymbolTableError::new(format!(
                        "Wrong table for argument: {:?}",
                        var
                    )));
                }
                self.table
                    .insert(var.name.clone(), (var.clone(), self.argument_count));
                self.argument_count += 1;
            }
        };
        Ok(())
    }

    pub fn add_vars_to_table(
        &mut self,
        table_check: &str,
        vars: &Vec<DeclareVariable>,
    ) -> Result<(), SymbolTableError> {
        for var in vars.iter() {
            SymbolTable::add_to_table(self, table_check, var)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn example_for_other_tests() -> ClassAndSubroutineTables {
        let mut sym_tables = ClassAndSubroutineTables::new("Class_name");
        use VarKinds::*;
        use VarTypes::*;

        // to class - examples from book - pg 225
        let cvar1 = DeclareVariable {
            name: "nAccounts".to_owned(),
            name_type: Int,
            kind: Static,
        };
        let cvar2 = DeclareVariable {
            name: "id".to_owned(),
            name_type: VarTypes::Int,
            kind: VarKinds::Field,
        };
        let cvar3 = DeclareVariable {
            name: "name".to_owned(),
            name_type: VarTypes::Class("String".to_owned()),
            kind: VarKinds::Field,
        };
        let cvar4 = DeclareVariable {
            name: "balance".to_owned(),
            name_type: VarTypes::Int,
            kind: VarKinds::Field,
        };

        // to subroutine & method call
        let svar1 = DeclareVariable {
            name: "this".to_owned(),
            name_type: VarTypes::Class("BankAccount".to_owned()),
            kind: VarKinds::Argument,
        };
        let svar2 = DeclareVariable {
            name: "sum".to_owned(),
            name_type: VarTypes::Int,
            kind: VarKinds::Argument,
        };
        let svar3 = DeclareVariable {
            name: "status".to_owned(),
            name_type: VarTypes::Boolean,
            kind: VarKinds::Local,
        };
        SymbolTable::add_vars_to_table(
            &mut sym_tables.0,
            "class",
            &vec![cvar1, cvar2, cvar3, cvar4],
        )
        .unwrap();

        // for i in [cvar1, cvar2, cvar3, cvar4].iter() {
        //     SymbolTable::add_to_table(&mut sym_tables.0, "class", i.clone()).unwrap();
        // }
        for j in [svar1, svar2, svar3].iter() {
            SymbolTable::add_to_table(&mut sym_tables.1, "subroutine", j).unwrap();
        }

        sym_tables
    }

    #[test]
    fn tables_test() {
        let sym_tables = example_for_other_tests();
        // checks - class
        assert!(
            ClassAndSubroutineTables::get(&sym_tables, "nAccounts").unwrap()
                == &(
                    DeclareVariable {
                        name: "nAccounts".to_owned(),
                        name_type: VarTypes::Int,
                        kind: VarKinds::Static
                    },
                    0
                )
        );
        // checks - subroutine
        assert!(
            ClassAndSubroutineTables::get(&sym_tables, "sum").unwrap()
                == &(
                    DeclareVariable {
                        name: "sum".to_owned(),
                        name_type: VarTypes::Int,
                        kind: VarKinds::Argument
                    },
                    1
                )
        );
    }

    #[test]
    fn counters_test() {
        let sym_tables = example_for_other_tests();

        // class
        assert!(sym_tables.0.static_count == 1);
        assert!(sym_tables.0.field_count == 3);
        // subroutine
        assert!(sym_tables.1.argument_count == 2);
        assert!(sym_tables.1.local_count == 1);
        // VarCount - i.e. same as above but in function form
        // assert!(ClassAndSubroutineTables::var_count(&sym_tables, VarKinds::Static) == 1);

        // // Specific DeclareVariable info
        // assert!(ClassAndSubroutineTables::kind_of(&sym_tables, "status").unwrap() == VarKinds::Local);
        // assert!(ClassAndSubroutineTables::type_of(&sym_tables, "status").unwrap() == VarTypes::Boolean);
        // assert!(ClassAndSubroutineTables::index_of(&sym_tables, "status").unwrap() == 0);
    }
    #[test]
    fn reset_subroutine_table() {
        let mut sym_tables = example_for_other_tests();
        ClassAndSubroutineTables::start_subroutine(&mut sym_tables);
        assert_eq!(SymbolTable::new(), sym_tables.1);
        assert_ne!(SymbolTable::new(), sym_tables.0);
    }
}
