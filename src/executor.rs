use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::parser::{self, BinaryOperator, Expression, Item};

#[derive(Debug)]
pub struct UserDefinedFunction {
    pub parameters: Vec<String>,
    pub body: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Function {
    #[debug("BuiltIn")]
    BuiltIn(Rc<dyn Fn(Vec<Value>) -> Result<Value, ExecutionError> + 'static>),
    #[debug("UserDefined")]
    UserDefined(Rc<UserDefinedFunction>),
}

#[derive(Debug, Clone)]
pub struct Object {
    pub properties: HashMap<String, Value>,
}

#[derive(Debug, Clone, Display)]
pub enum Value {
    #[display("undefined")]
    Undefined,
    #[display("{_0}")]
    Boolean(bool),
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
    Ok(Value::String(
        input[..input.len().saturating_sub(1)].to_string(),
    ))
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

    pub fn evaluate_function_call_expression(
        &mut self,
        func_call: &parser::FunctionCallExpression,
    ) -> Result<Value, ExecutionError> {
        let function_value = self
            .get_variable(&func_call.identifier)
            .ok_or_else(|| ExecutionError::UndefinedVariable(func_call.identifier.clone()))?
            // TODO: get around this clone? -> will be a pain
            .clone();
        let mut arg_values = Vec::new();
        for arg_expr in &func_call.arguments {
            let arg_value = self.evaluate_expression(arg_expr)?;
            arg_values.push(arg_value);
        }

        match function_value {
            Value::Function(Function::BuiltIn(func)) => func(arg_values),
            Value::Function(Function::UserDefined(func)) => {
                let mut new_scope = HashMap::new();
                let mut args = arg_values.into_iter();
                for param in func.parameters.iter() {
                    if let Some(arg_value) = args.next() {
                        new_scope.insert(param.clone(), arg_value);
                    } else {
                        new_scope.insert(param.clone(), Value::Undefined);
                    }
                }
                self.scope_stack.push(new_scope);

                let mut return_value = Value::Undefined;
                for item in func.body.iter() {
                    match item {
                        Item::ReturnStatement(expr) => {
                            return_value = if let Some(ret_expr) = &expr {
                                self.evaluate_expression(ret_expr)?
                            } else {
                                Value::Undefined
                            };
                            break;
                        }
                        _ => self.execute_item(item.clone())?,
                    }
                }
                self.scope_stack.pop();
                Ok(return_value)
            }
            _ => Err(ExecutionError::TypeMismatch(format!(
                "'{}' is not a function",
                func_call.identifier
            ))),
        }
    }

    pub fn evaluate_expression(&mut self, expr: &Expression) -> Result<Value, ExecutionError> {
        match expr {
            Expression::FloatLiteral(f) => Ok(Value::Float(*f)),
            Expression::IntegerLiteral(i) => Ok(Value::Integer(*i)),
            Expression::BinaryOperation {
                left,
                operator,
                right,
            } => {
                let left_value = self.evaluate_expression(left)?;
                let right_value = self.evaluate_expression(right)?;

                // do not format this match block, it is intentionally aligned for readability
                #[rustfmt::skip]
                let result = match (left_value, right_value, operator) {
                    // --- Integer Binary Operators ---
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
                    (Value::Integer(l), Value::Integer(r), BinaryOperator::Equal) => Ok(Value::Boolean(l == r)),
                    (Value::Integer(l), Value::Integer(r), BinaryOperator::NotEqual) => Ok(Value::Boolean(l != r)),
                    (Value::Integer(l), Value::Integer(r), BinaryOperator::LessThan) => Ok(Value::Boolean(l < r)),
                    (Value::Integer(l), Value::Integer(r), BinaryOperator::GreaterThan) => Ok(Value::Boolean(l > r)),
                    (Value::Integer(l), Value::Integer(r), BinaryOperator::LessThanOrEqual) => Ok(Value::Boolean(l <= r)),
                    (Value::Integer(l), Value::Integer(r), BinaryOperator::GreaterThanOrEqual) => Ok(Value::Boolean(l >= r)),

                    // --- Float Binary Operators ---
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
                    (Value::Float(l), Value::Float(r), BinaryOperator::Equal) => Ok(Value::Boolean(l == r)),
                    (Value::Float(l), Value::Float(r), BinaryOperator::NotEqual) => Ok(Value::Boolean(l != r)),
                    (Value::Float(l), Value::Float(r), BinaryOperator::LessThan) => Ok(Value::Boolean(l < r)),
                    (Value::Float(l), Value::Float(r), BinaryOperator::GreaterThan) => Ok(Value::Boolean(l > r)),
                    (Value::Float(l), Value::Float(r), BinaryOperator::LessThanOrEqual) => Ok(Value::Boolean(l <= r)),
                    (Value::Float(l), Value::Float(r), BinaryOperator::GreaterThanOrEqual) => Ok(Value::Boolean(l >= r)),

                    // --- Boolean Binary Operators ---
                    (Value::Boolean(l), Value::Boolean(r), BinaryOperator::Equal) => Ok(Value::Boolean(l == r)),
                    (Value::Boolean(l), Value::Boolean(r), BinaryOperator::NotEqual) => Ok(Value::Boolean(l != r)),
                    (Value::Boolean(l), Value::Boolean(r), BinaryOperator::And) => Ok(Value::Boolean(l && r)),
                    (Value::Boolean(l), Value::Boolean(r), BinaryOperator::Or) => Ok(Value::Boolean(l || r)),

                    // --- String Binary Operators ---
                    (Value::String(l), Value::String(r), BinaryOperator::Add) => Ok(Value::String(l + &r)),

                    // --- String-Integer Mixed Operators ---
                    (Value::String(l), Value::Integer(r), BinaryOperator::Add) => Ok(Value::String(l + &r.to_string())),
                    (Value::Integer(l), Value::String(r), BinaryOperator::Add) => Ok(Value::String(l.to_string() + &r)),

                    // -- String-Float Mixed Operators ---
                    (Value::String(l), Value::Float(r), BinaryOperator::Add) => Ok(Value::String(l + &r.to_string())),
                    (Value::Float(l), Value::String(r), BinaryOperator::Add) => Ok(Value::String(l.to_string() + &r)),

                    _ => Err(ExecutionError::TypeMismatch(format!(
                        "Cannot apply operator '{}' to given operand types",
                        operator
                    ))),
                };
                result
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
            Expression::Undefined => Ok(Value::Undefined),
            Expression::True => Ok(Value::Boolean(true)),
            Expression::False => Ok(Value::Boolean(false)),
            Expression::ObjectLiteral { properties } => {
                let mut obj_properties = HashMap::new();
                for (key, value_expr) in properties {
                    let value = self.evaluate_expression(value_expr)?;
                    obj_properties.insert(key.clone(), value);
                }
                let object = Object {
                    properties: obj_properties,
                };
                Ok(Value::Object(Rc::new(RefCell::new(object))))
            }
            Expression::FieldAccess { object, field } => {
                let object_value = self.evaluate_expression(object)?;
                match object_value {
                    Value::Object(obj_rc) => {
                        let obj_ref = obj_rc.borrow();
                        if let Some(field_value) = obj_ref.properties.get(field) {
                            Ok(field_value.clone())
                        } else {
                            Err(ExecutionError::UndefinedVariable(field.clone()))
                        }
                    }
                    _ => Err(ExecutionError::TypeMismatch(format!(
                        "Cannot access field '{}' on non-object value {:?}",
                        field, object_value
                    ))),
                }
            }
            _ => todo!("Expression evaluation not implemented for {:?}", expr),
        }
    }

    pub fn execute_item(&mut self, item: Item) -> Result<(), ExecutionError> {
        match item {
            Item::VariableDeclaration(var_decl) => {
                let value = if let Some(initializer) = var_decl.initializer {
                    self.evaluate_expression(&initializer)?
                } else {
                    Value::Undefined
                };

                self.declare_variable(var_decl.identifier, value);
                Ok(())
            }
            Item::VariableAssignment(var_assign) => {
                let value = self.evaluate_expression(&var_assign.value)?;
                self.assign_variable(var_assign.identifier, value)
            }
            Item::FunctionDeclaration(func_decl) => {
                let parameters = func_decl.parameters;
                let body = func_decl.body;
                let function = Function::UserDefined(Rc::new(UserDefinedFunction {
                    parameters,
                    body,
                }));

                let identifier = func_decl.identifier;
                self.declare_variable(identifier, Value::Function(function));
                Ok(())
            }
            Item::FunctionCallStatement(func_call) => {
                let _function_value = self.evaluate_function_call_expression(&func_call)?;
                Ok(())
            }
            // here: catch top-level returns, etc.
            _ => todo!("Item {:?} cannot be executed on its own", item),
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
        self.globals
            .insert("print".to_string(), Value::Function(print_function));

        let input_function = Function::BuiltIn(Rc::new(std_input));
        self.globals
            .insert("input".to_string(), Value::Function(input_function));
    }
}
