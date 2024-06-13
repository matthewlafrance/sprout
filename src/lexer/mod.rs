mod tokens;

use anyhow::{anyhow, Result};
use tokens::{Token, Literal, Punctuation};
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
            .or_else(|| self.read_punctuation()).map(Ok)
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
        let mut int_token = self.peek()?.to_digit(10)?;
        while let Some(int) = self.peek().and_then(|c| c.to_digit(10)) {
            int_token = int_token*10 + int;
            if self.consume_char().is_none() {break;}
        }
        Some(Token::Literal(Literal::Int(int_token)))
    }

    fn read_punctuation(&mut self) -> Option<Token> {
        Punctuation::try_from(self.peek()?).ok().map(Token::Punctuation)
    }

    fn peek(&self) -> Option<char> {
        self.current
    }

    fn consume_char(&mut self) -> Option<char> {
        self.current = self.src.next();
        self.current
    }
}
