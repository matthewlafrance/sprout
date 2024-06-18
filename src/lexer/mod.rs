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
            .or_else(|| self.read_rest())
            .map(Ok)
            .or_else(|| self.read_string_literal())
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
        while let Some(int) = self.consume_char().and_then(|c| c.to_digit(10)) { 
            if self.peek().is_none() {break;}
            int_token = int_token*10 + int;
        }
        Some(Token::Literal(Literal::Int(int_token)))
    }

    fn read_punctuation(&mut self) -> Option<Token> {
        self.consume_whitespace();
        Punctuation::try_from(self.peek()?).ok().map(|p| {
            self.consume_char();
            Token::Punctuation(p)})
    }

    fn read_string_literal(&mut self) -> Option<anyhow::Result<Token>> {
        self.consume_whitespace();
        match self.peek()? == '"' {
            true => self.consume_string_literal(),
            false => None,
        }
    }

    fn consume_string_literal(&mut self) -> Option<anyhow::Result<Token>> { 
        let mut string_literal = String::new();
        loop {
            match self.consume_char() {
                Some(c) => {
                    if c != '"' {     
                        string_literal.push(c);
                    } else {
                        break;
                    }
                },
                None => {return Some(Err(anyhow!("no closing delimiter on string literal")));}
            }
        }
        /*
        while self.consume_char()? != '"' {
            string_literal.push(self.peek()?);
        }
        */
        self.consume_char();
        Some(Ok(Token::Literal(Literal::Str(string_literal))))
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
        while let Some(c) = self.consume_char() {
            if c.is_alphanumeric() || c == '_' { 
                literal.push(c);
            } else {
                break;
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


mod tests {
    use crate::lexer::*;

    #[test]
    fn int_literal_test() {
        let result = Token::Literal(Literal::Int(123));
        let test: Vec<Token> = tokenize("123").map(|f| f.unwrap()).collect();
        assert_eq!(test[0], result);
    }

    #[test]
    fn test_ident() { 
        let result = Token::Ident("Test".to_string());
        let test: Vec<Token> = tokenize("Test").map(|f| f.unwrap()).collect();
        assert_eq!(test[0], result);
    }

    #[test]
    fn test_keyword() { 
        let result = Token::Keyword(Keyword::Mut);
        let test: Vec<Token> = tokenize("mut").map(|f| f.unwrap()).collect();
        assert_eq!(test[0], result);
    }

    #[test]
    fn test_punctuation() {        
        let result = Token::Punctuation(Punctuation::Equals);
        let test: Vec<Token> = tokenize("=").map(|f| f.unwrap()).collect();
        assert_eq!(test[0], result);
    }

    #[test]
    fn string_literal_test() {
        let result = Token::Literal(Literal::Str("Hello World".to_string()));
        let test: Vec<Token> = tokenize("\"Hello World\"").map(|f| f.unwrap()).collect();
        assert_eq!(test[0], result);
    }
    
    #[test]
    fn bool_literal_test() { 
        let result = Token::Literal(Literal::Bool(true));
        let test: Vec<Token> = tokenize("true").map(|f| f.unwrap()).collect();
        assert_eq!(test[0], result);
    }

    #[test]
    fn test_err_string_literal() {
        let result = "no closing delimiter on string literal".to_string();
        let test: Vec<String> = tokenize("\"hello world").map(|f| f.unwrap_err().to_string()).collect();
        assert_eq!(test[0], result);
    }
    
    #[test]
    fn test_all() {
        let test: Vec<Token> = tokenize("mut test = \"hello world\"; 123= true").map(|f| f.unwrap()).collect();
        assert_eq!(test[0], Token::Keyword(Keyword::Mut));
        assert_eq!(test[1], Token::Ident("test".to_string()));
        assert_eq!(test[2], Token::Punctuation(Punctuation::Equals));
        assert_eq!(test[3], Token::Literal(Literal::Str("hello world".to_string())));
        assert_eq!(test[4], Token::Punctuation(Punctuation::Semi));
        assert_eq!(test[5], Token::Literal(Literal::Int(123)));
        assert_eq!(test[6], Token::Punctuation(Punctuation::Equals));
        assert_eq!(test[7], Token::Literal(Literal::Bool(true)));
    }
}
