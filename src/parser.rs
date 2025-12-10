use crate::lexer::{Lexer, Token, TokenError};

#[derive(Debug, Display, Clone)]
pub enum BinaryOperator {
    #[display("+")]
    Add,
    #[display("-")]
    Subtract,
    #[display("*")]
    Multiply,
    #[display("/")]
    Divide,
    #[display("==")]
    Equal,
    #[display("!=")]
    NotEqual,
    #[display("<")]
    LessThan,
    #[display("<=")]
    LessThanOrEqual,
    #[display(">")]
    GreaterThan,
    #[display(">=")]
    GreaterThanOrEqual,
    #[display("&&")]
    And,
    #[display("||")]
    Or,
}

#[derive(Debug, Display, Clone)]
pub enum UnaryOperator {
    #[display("-")]
    Negate,
    #[display("!")]
    Not,
}

#[derive(Debug, Clone)]
pub struct FunctionCallExpression {
    pub identifier: String,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct FieldAccessExpression {
    pub object: Box<Expression>,
    pub field: String,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    True,
    False,
    Undefined,
    ObjectLiteral {
        properties: Vec<(String, Expression)>,
    },
    FieldAccess(FieldAccessExpression),
    FunctionCall(FunctionCallExpression),
    BinaryOperation {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    UnaryOperation {
        operator: UnaryOperator,
        operand: Box<Expression>,
    },
}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub identifier: String,
    pub initializer: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct VariableAssignment {
    pub identifier: String,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub identifier: String,
    pub parameters: Vec<String>,
    // TODO: support variadic parameters
    // pub variadic: Option<Vec<String>>,
    pub body: Vec<Item>,
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub condition: Expression,
    pub body: Vec<Item>,
    pub else_if_clauses: Vec<ElseIfClause>,
    pub else_clause: Option<ElseClause>,
}

#[derive(Debug, Clone)]
pub struct ElseIfClause {
    pub condition: Expression,
    pub body: Vec<Item>,
}

#[derive(Debug, Clone)]
pub struct ElseClause {
    pub body: Vec<Item>,
}

#[derive(Debug, Clone)]
pub struct FieldAssign {
    pub access: Expression,
    pub value: Expression,
}

#[derive(Debug, Clone)]
pub struct WhileExpression {
    pub condition: Expression,
    pub body: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Item {
    VariableDeclaration(VariableDeclaration),
    VariableAssignment(VariableAssignment),
    FunctionDeclaration(FunctionDeclaration),
    ReturnStatement(Option<Expression>),
    FunctionCallStatement(FunctionCallExpression),
    IfExpression(IfExpression),
    FieldAssign(FieldAssign),
    WhileExpression(WhileExpression),
    ContinueStatement,
}

#[derive(Debug)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, From, Display, Error)]
pub enum ParserError {
    LexerError(TokenError),
    #[display("Unexpected token: {_0}")]
    #[from(skip)]
    UnexpectedToken(#[error(not(source))] String),
    UnexpectedEOF,
}

#[derive(Debug, Clone, From)]
pub struct Parser<'source> {
    lexer: Lexer<'source>,
}

impl<'source> Parser<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            lexer: Lexer::new(source),
        }
    }

    fn consume_token(&mut self, expected: Token) -> Result<Token<'source>, ParserError> {
        let token = self
            .lexer
            .next_token()
            .ok_or(ParserError::UnexpectedEOF)??;

        if token == expected {
            Ok(token)
        } else {
            Err(ParserError::UnexpectedToken(format!("{:?}", token)))
        }
    }

    fn consume_identifier(&mut self) -> Result<String, ParserError> {
        let token = self
            .lexer
            .next_token()
            .ok_or(ParserError::UnexpectedEOF)??;

        if let Token::Identifier(name) = token {
            Ok(name.to_string())
        } else {
            Err(ParserError::UnexpectedToken(format!("{:?}", token)))
        }
    }

    fn parse_number_literal(num_str: &str) -> Result<Expression, ParserError> {
        if num_str.contains('.') {
            let value: f64 = num_str.parse().map_err(|_| {
                ParserError::UnexpectedToken(format!("Invalid float literal: {}", num_str))
            })?;
            Ok(Expression::FloatLiteral(value))
        } else {
            let value: i64 = num_str.parse().map_err(|_| {
                ParserError::UnexpectedToken(format!("Invalid integer literal: {}", num_str))
            })?;
            Ok(Expression::IntegerLiteral(value))
        }
    }

    fn parse_function_call_expression(&mut self) -> Result<FunctionCallExpression, ParserError> {
        let identifier_token = self
            .lexer
            .next_token()
            .ok_or(ParserError::UnexpectedEOF)??;
        let identifier = if let Token::Identifier(name) = identifier_token {
            name.to_string()
        } else {
            return Err(ParserError::UnexpectedToken(format!(
                "{:?}",
                identifier_token
            )));
        };
        let _lparen_token = self.consume_token(Token::LParen)?;
        let mut arguments = vec![];
        while let Some(token) = self.lexer.peek() {
            if token? == Token::RParen {
                break;
            }
            let argument = self.parse_expression()?;
            arguments.push(argument);
            if let Some(token) = self.lexer.peek() {
                if token? == Token::Comma {
                    let _comma_token = self.consume_token(Token::Comma)?;
                } else {
                    break;
                }
            }
        }
        let _rparen_token = self.consume_token(Token::RParen)?;
        Ok(FunctionCallExpression {
            identifier,
            arguments,
        })
    }

    fn parse_object_expression(&mut self) -> Result<Expression, ParserError> {
        let _double_lbrace_token = self.consume_token(Token::DoubleLBrace)?;
        let mut properties = vec![];
        while let Some(token) = self.lexer.peek() {
            if token? == Token::DoubleRBrace {
                break;
            }
            let key = self.consume_identifier()?;
            let _colon_token = self.consume_token(Token::Colon)?;
            let value = self.parse_expression()?;
            properties.push((key, value));
            if let Some(token) = self.lexer.peek() {
                if token? == Token::Comma {
                    let _comma_token = self.consume_token(Token::Comma)?;
                } else {
                    break;
                }
            }
        }
        let _double_rbrace_token = self.consume_token(Token::DoubleRBrace)?;
        Ok(Expression::ObjectLiteral { properties })
    }

    fn parse_if_expression(&mut self) -> Result<IfExpression, ParserError> {
        let _if_token = self.consume_token(Token::If)?;
        let condition = self.parse_expression()?;
        let body = self.parse_block()?;
        let mut else_if_clauses = vec![];
        while let Some(token) = self.lexer.peek() {
            if token? == Token::Elif {
                let _elif_token = self.consume_token(Token::Elif)?;
                let elif_condition = self.parse_expression()?;
                let elif_body = self.parse_block()?;
                else_if_clauses.push(ElseIfClause {
                    condition: elif_condition,
                    body: elif_body,
                });
            } else {
                break;
            }
        }
        let mut else_clause = None;
        if let Some(token) = self.lexer.peek() {
            if token? == Token::Else {
                let _else_token = self.consume_token(Token::Else)?;
                let else_body = self.parse_block()?;
                else_clause = Some(ElseClause { body: else_body });
            }
        }
        Ok(IfExpression {
            condition,
            body,
            else_if_clauses,
            else_clause,
        })
    }

    fn parse_while_expression(&mut self) -> Result<WhileExpression, ParserError> {
        let _while_token = self.consume_token(Token::While);
        let condition = self.parse_expression()?;
        let body = self.parse_block()?;
        Ok(WhileExpression { condition, body })
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, ParserError> {
        let token = self.lexer.peek().ok_or(ParserError::UnexpectedEOF)??;

        match token {
            Token::Identifier(name) => {
                // Check if it's a function call
                if let Some(next_token) = self.lexer.peek_n(1) {
                    if next_token? == Token::LParen {
                        let func_call = self.parse_function_call_expression()?;
                        return Ok(Expression::FunctionCall(func_call));
                    }
                }
                let _ = self.lexer.next_token(); // consume the identifier
                Ok(Expression::Identifier(name.to_string()))
            }
            Token::Number(num_str) => {
                let _ = self.lexer.next_token(); // consume the number
                Self::parse_number_literal(num_str)
            }
            Token::String(str_val) => {
                let _ = self.lexer.next_token(); // consume the string
                Ok(Expression::StringLiteral(str_val.to_string()))
            }
            Token::Undefined => {
                let _ = self.lexer.next_token(); // consume the undefined
                Ok(Expression::Undefined)
            }
            Token::True => {
                let _ = self.lexer.next_token(); // consume the true
                Ok(Expression::True)
            }
            Token::False => {
                let _ = self.lexer.next_token(); // consume the false
                Ok(Expression::False)
            }
            Token::DoubleLBrace => self.parse_object_expression(),
            Token::LParen => {
                self.consume_token(Token::LParen)?;
                let expr = self.parse_expression()?;
                self.consume_token(Token::RParen)?;
                Ok(expr)
            }
            _ => Err(ParserError::UnexpectedToken(format!("{:?}", token))),
        }
    }

    fn parse_field_access_expression(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_primary_expression()?;

        while let Some(token) = self.lexer.peek() {
            match token? {
                Token::Dot => {
                    let _dot_token = self.consume_token(Token::Dot)?;
                    let field_name = self.consume_identifier()?;
                    expr = Expression::FieldAccess(FieldAccessExpression {
                        object: Box::new(expr),
                        field: field_name,
                    });
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, ParserError> {
        let token = self.lexer.peek().ok_or(ParserError::UnexpectedEOF)??;

        match token {
            Token::Not | Token::Minus => {
                let operator_token = self
                    .lexer
                    .next_token()
                    .ok_or(ParserError::UnexpectedEOF)??;
                let operand = self.parse_unary_expression()?;
                match operator_token {
                    Token::Not => Ok(Expression::UnaryOperation {
                        operator: UnaryOperator::Not,
                        operand: Box::new(operand),
                    }),
                    Token::Minus => Ok(Expression::UnaryOperation {
                        operator: UnaryOperator::Negate,
                        operand: Box::new(operand),
                    }),
                    _ => unreachable!(),
                }
            }
            _ => self.parse_field_access_expression(),
        }
    }

    fn parse_factor_expression(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_unary_expression()?;
        while let Some(token) = self.lexer.peek() {
            match token? {
                Token::Asterisk | Token::Slash => {
                    let operator_token = self
                        .lexer
                        .next_token()
                        .ok_or(ParserError::UnexpectedEOF)??;
                    let right = self.parse_unary_expression()?;
                    let operator = match operator_token {
                        Token::Asterisk => BinaryOperator::Multiply,
                        Token::Slash => BinaryOperator::Divide,
                        _ => unreachable!(),
                    };
                    expr = Expression::BinaryOperation {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_term_expression(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_factor_expression()?;
        while let Some(token) = self.lexer.peek() {
            match token? {
                Token::Plus | Token::Minus => {
                    let operator_token = self
                        .lexer
                        .next_token()
                        .ok_or(ParserError::UnexpectedEOF)??;
                    let right = self.parse_factor_expression()?;
                    let operator = match operator_token {
                        Token::Plus => BinaryOperator::Add,
                        Token::Minus => BinaryOperator::Subtract,
                        _ => unreachable!(),
                    };
                    expr = Expression::BinaryOperation {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_comparison_expression(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_term_expression()?;

        while let Some(token) = self.lexer.peek() {
            match token? {
                Token::LessThan
                | Token::LessThanOrEqual
                | Token::GreaterThan
                | Token::GreaterThanOrEqual => {
                    let operator_token = self
                        .lexer
                        .next_token()
                        .ok_or(ParserError::UnexpectedEOF)??;
                    let right = self.parse_term_expression()?;
                    let operator = match operator_token {
                        Token::LessThan => BinaryOperator::LessThan,
                        Token::LessThanOrEqual => BinaryOperator::LessThanOrEqual,
                        Token::GreaterThan => BinaryOperator::GreaterThan,
                        Token::GreaterThanOrEqual => BinaryOperator::GreaterThanOrEqual,
                        _ => unreachable!(),
                    };
                    expr = Expression::BinaryOperation {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_equality_expression(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_comparison_expression()?;

        while let Some(token) = self.lexer.peek() {
            match token? {
                Token::DoubleEqual | Token::NotEqual => {
                    let operator_token = self
                        .lexer
                        .next_token()
                        .ok_or(ParserError::UnexpectedEOF)??;
                    let right = self.parse_comparison_expression()?;
                    let operator = match operator_token {
                        Token::DoubleEqual => BinaryOperator::Equal,
                        Token::NotEqual => BinaryOperator::NotEqual,
                        _ => unreachable!(),
                    };
                    expr = Expression::BinaryOperation {
                        left: Box::new(expr),
                        operator,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_logical_and_expression(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_equality_expression()?;

        while let Some(token) = self.lexer.peek() {
            match token? {
                Token::And => {
                    let _operator_token = self
                        .lexer
                        .next_token()
                        .ok_or(ParserError::UnexpectedEOF)??;
                    let right = self.parse_equality_expression()?;
                    expr = Expression::BinaryOperation {
                        left: Box::new(expr),
                        operator: BinaryOperator::And,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_logical_or_expression(&mut self) -> Result<Expression, ParserError> {
        let mut expr = self.parse_logical_and_expression()?;

        while let Some(token) = self.lexer.peek() {
            match token? {
                Token::Or => {
                    let _operator_token = self
                        .lexer
                        .next_token()
                        .ok_or(ParserError::UnexpectedEOF)??;
                    let right = self.parse_logical_and_expression()?;
                    expr = Expression::BinaryOperation {
                        left: Box::new(expr),
                        operator: BinaryOperator::Or,
                        right: Box::new(right),
                    };
                }
                _ => break,
            }
        }
        Ok(expr)
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_logical_or_expression()
    }

    fn parse_variable_declaration(&mut self) -> Result<VariableDeclaration, ParserError> {
        let _let_token = self.consume_token(Token::Let)?;
        let identifier_token = self
            .lexer
            .next_token()
            .ok_or(ParserError::UnexpectedEOF)??;
        let identifier = if let Token::Identifier(name) = identifier_token {
            name.to_string()
        } else {
            return Err(ParserError::UnexpectedToken(format!(
                "{:?}",
                identifier_token
            )));
        };
        let mut initializer = None;
        if let Some(token) = self.lexer.peek() {
            if token? == Token::Equal {
                let _equal_token = self.consume_token(Token::Equal)?;
                initializer = Some(self.parse_expression()?);
            }
        }
        let _semicolon_token = self.consume_token(Token::Semicolon)?;

        Ok(VariableDeclaration {
            identifier,
            initializer,
        })
    }

    fn parse_variable_assignment(&mut self) -> Result<VariableAssignment, ParserError> {
        let identifier_token = self
            .lexer
            .next_token()
            .ok_or(ParserError::UnexpectedEOF)??;
        let identifier = if let Token::Identifier(name) = identifier_token {
            name.to_string()
        } else {
            return Err(ParserError::UnexpectedToken(format!(
                "{:?}",
                identifier_token
            )));
        };
        let _equal_token = self.consume_token(Token::Equal)?;
        let value = self.parse_expression()?;
        let _semicolon_token = self.consume_token(Token::Semicolon)?;
        Ok(VariableAssignment { identifier, value })
    }

    fn parse_block(&mut self) -> Result<Vec<Item>, ParserError> {
        let _lbrace_token = self.consume_token(Token::LBrace)?;
        let mut items = vec![];
        while let Some(token) = self.lexer.peek() {
            if token? == Token::RBrace {
                break;
            }
            let item = self.parse_item()?;
            items.push(item);
        }
        let _rbrace_token = self.consume_token(Token::RBrace)?;
        Ok(items)
    }

    fn parse_function_declaration(&mut self) -> Result<FunctionDeclaration, ParserError> {
        let _fn_token = self.consume_token(Token::Fn)?;
        let identifier = self.consume_identifier()?;
        let _lparen_token = self.consume_token(Token::LParen)?;
        let mut parameters = vec![];
        while let Some(token) = self.lexer.peek() {
            if token? == Token::RParen {
                break;
            }
            let param_name = self.consume_identifier()?;
            parameters.push(param_name);
            if let Some(token) = self.lexer.peek() {
                if token? == Token::Comma {
                    let _comma_token = self.consume_token(Token::Comma)?;
                } else {
                    break;
                }
            }
        }
        let _rparen_token = self.consume_token(Token::RParen)?;
        let body = self.parse_block()?;
        Ok(FunctionDeclaration {
            identifier,
            parameters,
            body,
        })
    }

    fn parse_item(&mut self) -> Result<Item, ParserError> {
        let token = self.lexer.peek().ok_or(ParserError::UnexpectedEOF)??;

        match token {
            Token::Let => {
                let var_decl = self.parse_variable_declaration()?;
                Ok(Item::VariableDeclaration(var_decl))
            }
            Token::Fn => {
                let fn_decl = self.parse_function_declaration()?;
                Ok(Item::FunctionDeclaration(fn_decl))
            }
            Token::If => {
                let if_expr = self.parse_if_expression()?;
                Ok(Item::IfExpression(if_expr))
            }
            Token::While => {
                let while_expr = self.parse_while_expression()?;
                Ok(Item::WhileExpression(while_expr))
            }
            Token::Identifier(_) => {
                let next_token = self
                    .lexer
                    .peek_n_borrowed(1)
                    // if None, then EOF
                    .ok_or(ParserError::UnexpectedEOF)?
                    // if Err, clone the error and return it
                    .as_ref()
                    .map_err(|e| e.clone())?;

                match next_token {
                    Token::Equal => {
                        let var_assign = self.parse_variable_assignment()?;
                        Ok(Item::VariableAssignment(var_assign))
                    }
                    Token::LParen => {
                        let function_call = self.parse_function_call_expression()?;
                        let _semicolon_token = self.consume_token(Token::Semicolon)?;
                        Ok(Item::FunctionCallStatement(function_call))
                    }
                    Token::Dot => {
                        let field_access_expr = self.parse_field_access_expression()?;
                        let _equal_token = self.consume_token(Token::Equal)?;
                        let value_expr = self.parse_expression()?;
                        let _semicolon_token = self.consume_token(Token::Semicolon)?;
                        Ok(Item::FieldAssign(FieldAssign {
                            access: field_access_expr,
                            value: value_expr,
                        }))
                    }
                    _ => return Err(ParserError::UnexpectedToken(format!("{:?}", next_token))),
                }
            }
            Token::Return => {
                let _return_token = self.consume_token(Token::Return)?;
                let expr =
                    if self.lexer.peek().ok_or(ParserError::UnexpectedEOF)?? != Token::Semicolon {
                        Some(self.parse_expression()?)
                    } else {
                        None
                    };
                let _semicolon_token = self.consume_token(Token::Semicolon)?;
                Ok(Item::ReturnStatement(expr))
            }
            Token::Continue => {
                let _continue_token = self.consume_token(Token::Continue)?;
                let _semicolon_token = self.consume_token(Token::Semicolon)?;
                Ok(Item::ContinueStatement)
            }
            t => return Err(ParserError::UnexpectedToken(format!("{:?}", t))),
        }
    }

    pub fn parse_module(&mut self) -> Result<Module, ParserError> {
        let mut items = vec![];
        while !self.lexer.exhausted() {
            let item = self.parse_item()?;
            items.push(item);
        }
        Ok(Module { items })
    }

    pub fn position(&self) -> usize {
        self.lexer.position()
    }
}
