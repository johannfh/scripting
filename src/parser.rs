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
pub enum Expression {
    Identifier(String),
    IntegerLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
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
pub enum Item {
    VariableDeclaration(VariableDeclaration),
    VariableAssignment(VariableAssignment),
    FunctionCallStatement(FunctionCallExpression),
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

    fn parse_number_literal(
        num_str: &str,
    ) -> Result<Expression, ParserError> {
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
        let identifier_token = self.lexer
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
        Ok(FunctionCallExpression { identifier, arguments })
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, ParserError> {
        let token = self
            .lexer
            .peek()
            .ok_or(ParserError::UnexpectedEOF)??;

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
            },
            Token::String(str_val) => {
                let _ = self.lexer.next_token(); // consume the string
                Ok(Expression::StringLiteral(str_val.to_string()))
            }
            Token::LParen => {
                self.consume_token(Token::LParen)?;
                let expr = self.parse_expression()?;
                self.consume_token(Token::RParen)?;
                Ok(expr)
            }
            _ => Err(ParserError::UnexpectedToken(format!("{:?}", token))),
        }
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, ParserError> {
        let token = self
            .lexer
            .peek()
            .ok_or(ParserError::UnexpectedEOF)??;

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
            _ => self.parse_primary_expression(),
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
        self.parse_term_expression()
    }

    fn parse_equality_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_comparison_expression()
    }

    fn parse_expression(&mut self) -> Result<Expression, ParserError> {
        self.parse_equality_expression()
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

    fn parse_item(&mut self) -> Result<Item, ParserError> {
        let token = self
            .lexer
            .peek()
            .ok_or(ParserError::UnexpectedEOF)??;

        match token {
            Token::Let => {
                let var_decl = self.parse_variable_declaration()?;
                Ok(Item::VariableDeclaration(var_decl))
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
                    _ => {
                        return Err(ParserError::UnexpectedToken(format!(
                            "{:?}",
                            next_token
                        )))
                    }
                }

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
