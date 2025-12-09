use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::parser::{self, BinaryOperator, Expression};

#[derive(Debug, Clone)]
pub enum Function {
    #[debug("BuiltIn")]
    BuiltIn(Rc<dyn Fn(Vec<Value>) -> Result<Value, ExecutionError> + 'static>),
    #[debug("UserDefined")]
    UserDefined {
        parameters: Vec<String>,
        body: Vec<parser::Item>,
    },
}

#[derive(Debug, Clone)]
pub struct Object {
    pub properties: HashMap<String, Value>,
}

#[derive(Debug, Clone, Display)]
pub enum Value {
    #[display("{_0}")]
    Integer(i64),
    #[display("{_0}")]
    Float(f64),
    #[display("{_0}")]
    String(String),
    #[display("<function>")]
    Function(Function),
    // WARN: This can lead to reference cycles if not handled carefully
    // Make sure to manage object references properly to avoid memory leaks
    // TODO: Consider using weak references or a sophisticated garbage collection mechanism
    #[display("<object {{{:?}}}>", _0.borrow().properties)]
    Object(Rc<RefCell<Object>>),
    #[display("undefined")]
    Undefined,
}

#[derive(Debug, Display, From, Error)]
pub enum ExecutionError {
    #[from]
    IoError(std::io::Error),
    UndefinedVariable(#[error(not(source))] String),
    TypeMismatch(#[error(not(source))] String),
    DivisionByZero,
    Other(#[error(not(source))] String),
}

fn std_print(args: Vec<Value>) -> Result<Value, ExecutionError> {
    for arg in args {
        print!("{}", arg);
    }
    println!();
    Ok(Value::Undefined)
}

fn std_input(args: Vec<Value>) -> Result<Value, ExecutionError> {
    use std::io::{self, Write};

    for arg in args {
        print!("{}", arg);
    }

    io::stdout().flush()?;

    let mut input = String::new();
    io::stdin().read_line(&mut input)?;
    Ok(Value::String(input[..input.len().saturating_sub(1)].to_string()))
}

#[derive(Debug, Default)]
pub struct Executor {
    globals: HashMap<String, Value>,
    scope_stack: Vec<HashMap<String, Value>>,
}

impl Executor {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn declare_variable(&mut self, name: String, value: Value) {
        if let Some(scope) = self.scope_stack.last_mut() {
            scope.insert(name, value);
        } else {
            self.globals.insert(name, value);
        }
    }

    pub fn assign_variable(&mut self, name: String, value: Value) -> Result<(), ExecutionError> {
        if let Some(scope) = self.scope_stack.last_mut() {
            if let Some(var) = scope.get_mut(&name) {
                *var = value;
                Ok(())
            } else {
                Err(ExecutionError::UndefinedVariable(name))
            }
        } else {
            if let Some(var) = self.globals.get_mut(&name) {
                *var = value;
                Ok(())
            } else {
                Err(ExecutionError::UndefinedVariable(name))
            }
        }
    }

    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        self.globals.get(name)
    }

    pub fn evaluate_function_call_expression(&self, func_call: &parser::FunctionCallExpression) -> Result<Value, ExecutionError> {
        let function_value = self.get_variable(&func_call.identifier)
            .ok_or_else(|| ExecutionError::UndefinedVariable(func_call.identifier.clone()))?;
        let mut arg_values = Vec::new();
        for arg_expr in &func_call.arguments {
            let arg_value = self.evaluate_expression(arg_expr)?;
            arg_values.push(arg_value);
        }

        match function_value {
            Value::Function(Function::BuiltIn(func)) => {
                func(arg_values)
            }
            Value::Function(Function::UserDefined { .. }) => {
                todo!("User-defined function execution not implemented yet");
            }
            _ => Err(ExecutionError::TypeMismatch(format!(
                "'{}' is not a function",
                func_call.identifier
            ))),
        }
    }

    pub fn evaluate_expression(&self, expr: &Expression) -> Result<Value, ExecutionError> {
        match expr {
            Expression::FloatLiteral(f) => Ok(Value::Float(*f)),
            Expression::IntegerLiteral(i) => Ok(Value::Integer(*i)),
            Expression::BinaryOperation { left, operator, right } => {
                let left_value = self.evaluate_expression(left)?;
                let right_value = self.evaluate_expression(right)?;
                match (left_value, right_value, operator) {
                    (Value::Integer(l), Value::Integer(r), BinaryOperator::Add) => Ok(Value::Integer(l + r)),
                    (Value::Integer(l), Value::Integer(r), BinaryOperator::Subtract) => Ok(Value::Integer(l - r)),
                    (Value::Integer(l), Value::Integer(r), BinaryOperator::Multiply) => Ok(Value::Integer(l * r)),
                    (Value::Integer(l), Value::Integer(r), BinaryOperator::Divide) => {
                        if r == 0 {
                            Err(ExecutionError::DivisionByZero)
                        } else {
                            Ok(Value::Integer(l / r))
                        }
                    }
                    (Value::Float(l), Value::Float(r), BinaryOperator::Add) => Ok(Value::Float(l + r)),
                    (Value::Float(l), Value::Float(r), BinaryOperator::Subtract) => Ok(Value::Float(l - r)),
                    (Value::Float(l), Value::Float(r), BinaryOperator::Multiply) => Ok(Value::Float(l * r)),
                    (Value::Float(l), Value::Float(r), BinaryOperator::Divide) => {
                        if r == 0.0 {
                            Err(ExecutionError::DivisionByZero)
                        } else {
                            Ok(Value::Float(l / r))
                        }
                    }
                    (Value::String(l), Value::String(r), BinaryOperator::Add) => Ok(Value::String(l + &r)),
                    _ => Err(ExecutionError::TypeMismatch(format!(
                        "Cannot apply operator '{}' to given operand types",
                        operator
                    ))),
                }
            }
            Expression::StringLiteral(s) => Ok(Value::String(s.clone())),
            Expression::Identifier(name) => {
                if let Some(value) = self.get_variable(name) {
                    Ok(value.clone())
                } else {
                    Err(ExecutionError::UndefinedVariable(name.clone()))
                }
            }
            Expression::FunctionCall(func_call) => {
                self.evaluate_function_call_expression(func_call)
            }
            _ => todo!("Expression evaluation not implemented for {:?}", expr),
        }
    }

    pub fn execute_item(&mut self, item: parser::Item) -> Result<(), ExecutionError> {
        match item {
            parser::Item::VariableDeclaration(var_decl) => {
                let value = if let Some(initializer) = var_decl.initializer {
                    self.evaluate_expression(&initializer)?
                } else {
                    Value::Undefined
                };

                self.declare_variable(var_decl.identifier, value);
                Ok(())
            }
            parser::Item::VariableAssignment(var_assign) => {
                let value = self.evaluate_expression(&var_assign.value)?;
                self.assign_variable(var_assign.identifier, value)
            }
            parser::Item::FunctionCallStatement(func_call) => {
                let _function_value = self.evaluate_function_call_expression(&func_call)?;
                Ok(())
            }
        }
    }

    pub fn execute_module(&mut self, module: parser::Module) -> Result<(), ExecutionError> {
        for item in module.items {
            self.execute_item(item)?;
        }
        Ok(())
    }

    pub fn load_standard_library(&mut self) {
        info!("Loading standard library functions...");

        let print_function = Function::BuiltIn(Rc::new(std_print));
        self.globals.insert("print".to_string(), Value::Function(print_function));

        let input_function = Function::BuiltIn(Rc::new(std_input));
        self.globals.insert("input".to_string(), Value::Function(input_function));
    }
}
