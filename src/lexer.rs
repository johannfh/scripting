use std::collections::VecDeque;

use logos::Logos;


#[derive(Debug, Clone, PartialEq, Logos)]
#[logos(skip(r"[ \t\n\f]+"))]
#[logos(skip(r"//[^\n]*"))]
#[logos(error = TokenError)]
pub enum Token<'source> {
    #[token("let")]
    Let,
    #[token("fn")]
    Fn,
    #[token("return")]
    Return,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("in")]
    In,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("true")]
    True,
    #[token("false")]
    False,
    #[token("undefined")]
    Undefined,

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier(&'source str),

    #[regex(r"[0-9]+(\.[0-9]+)?")]
    Number(&'source str),

    #[regex(r#"('[^']*')|("[^"]*")"#, |lex| {
        let slice = lex.slice();
        &slice[1..slice.len()-1]
    })]
    String(&'source str),

    #[token("=")]
    Equal,
    #[token("==")]
    DoubleEqual,
    #[token("+")]
    Plus,
    #[token("+=")]
    PlusEqual,
    #[token("-")]
    Minus,
    #[token("-=")]
    MinusEqual,
    #[token("*")]
    Asterisk,
    #[token("*=")]
    AsteriskEqual,
    #[token("/")]
    Slash,
    #[token("/=")]
    SlashEqual,
    #[token("!")]
    Not,
    #[token("!=")]
    NotEqual,

    #[token("<")]
    LessThan,
    #[token("<=")]
    LessThanOrEqual,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterThanOrEqual,

    #[token("&&")]
    And,
    #[token("||")]
    Or,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("{{")]
    DoubleLBrace,
    #[token("}}")]
    DoubleRBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token(".")]
    Dot,
    #[token(",")]
    Comma,
    #[token("->")]
    Arrow,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token(";")]
    Semicolon,
}

#[derive(Debug, Clone)]
pub struct Lexer<'source> {
    logos_lexer: logos::Lexer<'source, Token<'source>>,
    peeked_deque: VecDeque<Result<Token<'source>, TokenError>>,
}

#[derive(Debug, Clone, PartialEq, Default, Display, Error)]
pub struct TokenError;

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            logos_lexer: Token::lexer(source),
            peeked_deque: VecDeque::new(),
        }
    }

    pub fn next_token(&mut self) -> Option<Result<Token<'source>, TokenError>> {
        if let Some(token) = self.peeked_deque.pop_front() {
            Some(token)
        } else {
            self.logos_lexer.next()
        }
    }

    pub fn peek_borrowed(&mut self) -> Option<&Result<Token<'source>, TokenError>> {
        if self.peeked_deque.is_empty() {
            if let Some(token) = self.logos_lexer.next() {
                self.peeked_deque.push_back(token);
            }
        }
        self.peeked_deque.front()
    }

    pub fn peek(&mut self) -> Option<Result<Token<'source>, TokenError>> {
        self.peek_borrowed().cloned()
    }

    pub fn peek_n_borrowed(&mut self, n: usize) -> Option<&Result<Token<'source>, TokenError>> {
        while self.peeked_deque.len() <= n {
            if let Some(token) = self.logos_lexer.next() {
                self.peeked_deque.push_back(token);
            } else {
                break;
            }
        }
        self.peeked_deque.get(n)
    }

    pub fn peek_n(&mut self, n: usize) -> Option<Result<Token<'source>, TokenError>> {
        self.peek_n_borrowed(n).cloned()
    }

    pub fn position(&self) -> usize {
        self.logos_lexer.span().start
    }

    pub fn exhausted(&mut self) -> bool {
        self.peek().is_none()
    }
}
