use crate::lexer::tokens::Literal;

#[derive(PartialEq, Debug)]
pub struct Module {
    pub funcs: Vec<Func>,
}

#[derive(PartialEq, Debug)]
pub struct Func {
    pub name: String,
    pub param_list: Vec<Param>,
    pub return_type: Option<Type>,
    pub block: Block,
}

#[derive(PartialEq, Debug)]
pub struct Param {
    pub name: String,
    pub param_type: Type,
}

#[derive(PartialEq, Debug)]
pub struct Block {
    pub stmts: Vec<Statement>,
}

#[derive(PartialEq, Debug)]
pub enum Statement {
    VarDecl(VarDecl),
    Assignment(Assignment),
    Conditional(Conditional),
    Break,
    Continue,
    RepeatLoop(RepeatLoop),
    WhileLoop(WhileLoop),
    FunctionCall(FunctionCall),
    Return(Return),
}

#[derive(PartialEq, Debug)]
pub struct Return {
    pub value: Expr,
}

#[derive(PartialEq, Debug)]
pub struct VarDecl {
    pub mutable: bool,
    pub name: String,
    pub var_type: Option<Type>,
    pub value: Option<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct Assignment {
    pub name: String,
    pub value: Expr,
}

#[derive(PartialEq, Debug)]
pub struct Conditional {
    pub condition: Expr,
    pub if_block: Block,
    pub else_block: Option<Block>,
}

#[derive(PartialEq, Debug)]
pub struct RepeatLoop {
    pub body: Block,
}

#[derive(PartialEq, Debug)]
pub struct WhileLoop {
    pub condition: Expr,
    pub body: Block,
}

#[derive(PartialEq, Debug)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Expr>,
}

#[derive(PartialEq, Debug)]
pub struct Type {
    pub name: String,
}

#[derive(PartialEq, Debug)]
pub enum Expr {
    Literal(Literal),
    FunctionCall(FunctionCall),
    Ident(String),
    UnaryOp(Vec<UnaryOp>, Box<Expr>),
    BinaryOp(Box<Expr>, BinaryOp, Box<Expr>),
}

#[derive(PartialEq, Debug)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mul,
    Div,
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
}

#[derive(PartialEq, Debug)]
pub enum UnaryOp {
    Neg,
}
