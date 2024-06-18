use soccer::TryFrom;

#[derive(PartialEq, Debug)]
pub enum Token {
    Ident(String),
    Keyword(Keyword),
    Literal(Literal),
    Punctuation(Punctuation),
}

#[derive(PartialEq, Debug)]
pub enum Keyword {
    Let,
    Mut,
    Print,
    Read,
    Func,
    Repeat,
    While,
    If,
    Else,
}

#[derive(PartialEq, Debug)]
pub enum Literal {
    Int(u32),
    Str(String),
    Bool(bool),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, TryFrom)]
#[const_ty(char)]
pub enum Punctuation {
    #[const_val('+')]
    Plus,
    #[const_val('-')]
    Minus,
    #[const_val('*')]
    Star,
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
    #[const_val('{')]
    LeftBrace,
    #[const_val('}')]
    RightBrace,
    #[const_val('(')]
    LeftParen,
    #[const_val(')')]
    RightParen,
}
