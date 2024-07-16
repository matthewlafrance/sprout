use soccer::{Display, Into, TryFrom};
use std::fmt;

#[derive(PartialEq, Debug)]
pub enum Token {
    Ident(String),
    Keyword(Keyword),
    Literal(Literal),
    Punctuation(Punctuation),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Ident(s) => write!(f, "{s}"),
            Token::Keyword(k) => write!(f, "{k}"),
            Token::Literal(l) => match l {
                Literal::Int(i) => write!(f, "{i}"),
                Literal::Str(s) => write!(f, "{s}"),
                Literal::Bool(b) => write!(f, "{b}"),
            },
            Token::Punctuation(p) => write!(f, "{p}"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, TryFrom, Into, Display)]
#[const_ty(&'static str)]
pub enum Keyword {
    #[const_val("let")]
    Let,
    #[const_val("mut")]
    Mut,
    #[const_val("func")]
    Func,
    #[const_val("repeat")]
    Repeat,
    #[const_val("while")]
    While,
    #[const_val("if")]
    If,
    #[const_val("else")]
    Else,
    #[const_val("break")]
    Break,
    #[const_val("continue")]
    Continue,
    #[const_val("return")]
    Return,
}

#[derive(PartialEq, Debug)]
pub enum Literal {
    Int(u32),
    Str(String),
    Bool(bool),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, TryFrom, Into, Display)]
#[const_ty(char)]
pub enum Punctuation {
    #[const_val('+')]
    Plus,
    #[const_val('-')]
    Minus,
    #[const_val('*')]
    Star,
    #[const_val('/')]
    Div,
    #[const_val('=')]
    Equals,
    #[const_val('<')]
    LeftAngle,
    #[const_val('>')]
    RightAngle,
    #[const_val('!')]
    Bang,
    #[const_val(';')]
    Semi,
    #[const_val(':')]
    Colon,
    #[const_val(',')]
    Comma,
    #[const_val('{')]
    LeftBrace,
    #[const_val('}')]
    RightBrace,
    #[const_val('(')]
    LeftParen,
    #[const_val(')')]
    RightParen,
}
