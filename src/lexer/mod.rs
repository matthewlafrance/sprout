mod tokens;

use anyhow::{anyhow, Result};
use tokens::{Token, Literal, Punctuation, Keyword};
use std::str::Chars;

pub fn tokenize(src: &str) -> impl Iterator<Item = anyhow::Result<Token>> + '_ {
    //parse through src by character
    //filter based on possible tokens
    //when i reach a new token, check first character and match with possibilities
        //if digit, int
        //if puncuation, punctuation
        //if ", string
        //if letter, then get entire token and decide between bool, keyword, ident
    //parse rest of token and store accordingly

    TokenStream::new(src)
}

struct TokenStream<'a> {
    // add fields
    src: Chars<'a>,
    current: Option<char>,
}

impl Iterator for TokenStream<'_> {
    type Item = anyhow::Result<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        self
            .read_int_literal()
            .or_else(|| self.read_punctuation())
            .or_else(|| self.read_string_literal())
            .or_else(|| self.read_rest())
            .map(Ok)
            .or_else(|| self.peek().map(|c| Err(anyhow!("invalid character: {c}"))))
    }
}

impl<'a> TokenStream<'a> {
    fn new(src: &'a str) -> Self {
        let mut src = src.chars();
        let current = src.next();
        TokenStream {src, current} 
    }

    fn read_int_literal(&mut self) -> Option<Token> {
        self.consume_whitespace();
        let mut int_token = self.peek()?.to_digit(10)?;
        while let Some(int) = self.peek().and_then(|c| c.to_digit(10)) {
            int_token = int_token*10 + int;
            if self.consume_char().is_none() {break;}
        }
        Some(Token::Literal(Literal::Int(int_token)))
    }

    fn read_punctuation(&mut self) -> Option<Token> {
        self.consume_whitespace();
        Punctuation::try_from(self.peek()?).ok().map(Token::Punctuation)
    }

    fn read_string_literal(&mut self) -> Option<Token> {
        self.consume_whitespace();
        match self.peek()? == '"' {
            true => self.consume_string_literal(),
            false => None,
        }
    }

    fn consume_string_literal(&mut self) -> Option<Token> {
        let mut string_literal = String::from(self.peek()?);
        loop {
            match self.consume_char()? {
                '"' => {break;},
                _ => {string_literal.push(self.peek()?);},
            }
        }
        Some(Token::Literal(Literal::Str(string_literal)))
    }

    fn read_rest(&mut self) -> Option<Token> {
        self.consume_whitespace();
        match self.peek()?.is_alphabetic() || self.peek()? == '_' {
            true => self.consume_rest(),
            false => None,
        }
    }

    fn consume_rest(&mut self) -> Option<Token> {
        let mut literal = String::from(self.peek()?);
        loop {
            match self.consume_char()?.is_alphanumeric() || self.peek()? == '_' {
                true => {literal.push(self.peek()?);},
                false => {break;}
            }
        }
        match literal.as_str() {
            "let" => Some(Token::Keyword(Keyword::Let)),
            "mut" => Some(Token::Keyword(Keyword::Mut)),
            "print" => Some(Token::Keyword(Keyword::Print)),
            "read" => Some(Token::Keyword(Keyword::Read)),
            "func" => Some(Token::Keyword(Keyword::Func)),
            "repeat" => Some(Token::Keyword(Keyword::Repeat)),
            "while" => Some(Token::Keyword(Keyword::Repeat)),
            "if" => Some(Token::Keyword(Keyword::If)),
            "else" => Some(Token::Keyword(Keyword::Else)),
            "true" => Some(Token::Literal(Literal::Bool(true))),
            "false" => Some(Token::Literal(Literal::Bool(false))),
            ident => Some(Token::Ident(ident.to_string())),
        }
    }

    fn consume_whitespace(&mut self) {
        loop {
            match self.peek() {
                Some(c) => {
                    match c.is_whitespace() {
                        true => {self.consume_char();}
                        false => {break;}
                    }
                },
                None => {break;},
            }
        }
    }

    fn peek(&self) -> Option<char> {
        self.current
    }

    fn consume_char(&mut self) -> Option<char> {
        self.current = self.src.next();
        self.current
    }
}
