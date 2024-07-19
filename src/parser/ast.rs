use crate::lexer::tokens::Literal;
use std::fmt;
use soccer::{Display, Into, TryFrom};

#[derive(PartialEq, Debug)]
pub struct Module {
    pub funcs: Vec<Func>,
}

impl Module {
    fn peek_funcs(&self) -> &Vec<Func> {
        &self.funcs
    }
}

#[derive(PartialEq, Debug)]
pub struct Func {
    pub name: String,
    pub param_list: Vec<Param>,
    pub return_type: Option<Type>,
    pub block: Block,
}

impl Func {
    fn peek_name(&self) -> &str {
        &self.name
    }

    fn peek_param_list(&self) -> &Vec<Param> {
        &self.param_list
    }

    fn peek_return_type(&self) -> &Option<Type> {
        &self.return_type
    }

    fn peek_block(&self) -> &Block {
        &self.block
    }
}

#[derive(PartialEq, Debug)]
pub struct Param {
    pub name: String,
    pub param_type: Type,
}

impl Param {
    fn peek_name(&self) -> &str {
        &self.name
    }

    fn peek_param_type(&self) -> &Type {
        &self.param_type
    }
}

#[derive(PartialEq, Debug)]
pub struct Block {
    pub stmts: Vec<Statement>,
}

impl Block {
    fn peek_stmts(&self) -> &Vec<Statement> {
        &self.stmts
    }
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
pub struct VarDecl {
    pub mutable: bool,
    pub name: String,
    pub var_type: Option<Type>,
    pub value: Option<Expr>,
}

impl VarDecl {
    fn peek_mutable(&self) -> bool {
        self.mutable
    }

    fn peek_name(&self) -> &str {
        &self.name
    }

    fn peek_var_type(&self) -> &Option<Type> {
        &self.var_type
    }

    fn peek_value(&self) -> &Option<Expr> {
        &self.value
    }
}

#[derive(PartialEq, Debug)]
pub struct Assignment {
    pub name: String,
    pub value: Expr,
}

impl Assignment {
    fn peek_name(&self) -> &str {
        &self.name
    }

    fn peek_value(&self) -> &Expr {
        &self.value
    }
}

#[derive(PartialEq, Debug)]
pub struct Conditional {
    pub condition: Expr,
    pub if_block: Block,
    pub else_block: Option<Block>,
}

impl Conditional {
    fn peek_condition(&self) -> &Expr {
        &self.condition
    }

    fn peek_if_block(&self) -> &Block {
        &self.if_block
    }

    fn peek_else_block(&self) -> &Option<Block> {
        &self.else_block
    }
}

#[derive(PartialEq, Debug)]
pub struct RepeatLoop {
    pub body: Block,
}

impl RepeatLoop {
    fn peek_body(&self) -> &Block {
        &self.body
    }
}

#[derive(PartialEq, Debug)]
pub struct WhileLoop {
    pub condition: Expr,
    pub body: Block,
}

impl WhileLoop {
    fn peek_condition(&self) -> &Expr {
        &self.condition
    }

    fn peek_body(&self) -> &Block {
        &self.body
    }
}

#[derive(PartialEq, Debug)]
pub struct FunctionCall {
    pub name: String,
    pub args: Vec<Expr>,
}

impl FunctionCall {
    fn peek_name(&self) -> &str {
        &self.name
    }

    fn peek_args(&self) -> &Vec<Expr> {
        &self.args
    }
}

#[derive(PartialEq, Debug)]
pub struct Return {
    pub value: Expr,
}

impl Return {
    fn peek_value(&self) -> &Expr {
        &self.value
    }
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, TryFrom, Into, Display)]
#[const_ty(&'static str)]
pub enum BinaryOp {
    #[const_val("+")]
    Plus,
    #[const_val("-")]
    Minus,
    #[const_val("*")]
    Mul,
    #[const_val("/")]
    Div,
    #[const_val("==")]
    Eq,
    #[const_val("!=")]
    Ne,
    #[const_val("<=")]
    Le,
    #[const_val(">=")]
    Ge,
    #[const_val("<")]
    Lt,
    #[const_val(">")]
    Gt,
}

#[derive(PartialEq, Debug)]
pub enum UnaryOp {
    Neg,
}

pub struct PrintVisitor {
    pub ast: Module,
}

impl fmt::Display for PrintVisitor {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut buf = String::new();
        PrintVisitor::visit_module(&self.ast, &mut buf);
        write!(f, "{}", buf)
    }
}

impl AstVisitor for PrintVisitor {
    fn visit_module(module: &Module, buf: &mut String) -> anyhow::Result<()> {
        for f in module.peek_funcs() {
            Self::visit_func(&f, buf)?;
        }
        Ok(())
    }

    fn visit_func(func: &Func, buf: &mut String) -> anyhow::Result<()> {
        buf.push_str("func ");
        buf.push_str(func.peek_name());
        buf.push_str("(");
        let mut params = func.peek_param_list().iter();
        if let Some(p) = params.next() {
            Self::visit_param(&p, buf);
            for param in params {
                buf.push_str(", ");
                Self::visit_param(&param, buf)?;
            }
        }
        buf.push_str(")");
        if let Some(t) = func.peek_return_type() {
            Self::visit_type(&t, buf)?;
        }
        buf.push_str(" ");
        let block = func.peek_block();
        Self::visit_block(block, 0, buf)?;
        buf.push_str("\n");
        Ok(())
    }

    fn visit_param(param: &Param, buf: &mut String) -> anyhow::Result<()> {
        buf.push_str(param.peek_name());
        Self::visit_type(&param.peek_param_type(), buf);
        Ok(())
    }

    fn visit_type(t: &Type, buf: &mut String) -> anyhow::Result<()> {
        buf.push_str(": ");
        buf.push_str(&t.name);
        Ok(())
    }

    fn visit_block(block: &Block, depth: usize, buf: &mut String) -> anyhow::Result<()> {
        buf.push_str("{\n");
        for stmt in block.peek_stmts() {
            Self::visit_stmt(&stmt, depth+1, buf);
        }
        buf.push_str(&" ".repeat(depth*4));
        buf.push_str("}");
        Ok(())
    }

    fn visit_stmt(stmt: &Statement, depth: usize, buf: &mut String) -> anyhow::Result<()> {
        match stmt {
            Statement::VarDecl(v) => {Self::visit_var_decl(v, depth, buf)?;},
            Statement::Assignment(a) => {Self::visit_assignment(a, depth, buf)?;},
            Statement::Conditional(c) => {Self::visit_conditional(c, depth, buf)?;},
            Statement::Break => {
                buf.push_str(&" ".repeat(depth*4));
                buf.push_str("break;\n");
            },
            Statement::Continue => {
                buf.push_str(&" ".repeat(depth*4));
                buf.push_str("continue;\n");
            },
            Statement::RepeatLoop(r) => {Self::visit_repeat_loop(r, depth, buf)?;},
            Statement::WhileLoop(w) => {Self::visit_while_loop(w, depth, buf);},
            Statement::FunctionCall(f) => {
                buf.push_str(&" ".repeat(depth*4));
                Self::visit_function_call(f, buf)?;
                buf.push_str(";\n");
            },
            Statement::Return(r) => {Self::visit_return(r, depth, buf)?;},
        }
        Ok(())
    }

    fn visit_var_decl(var_decl: &VarDecl, depth: usize, buf: &mut String) -> anyhow::Result<()> {
        buf.push_str(&" ".repeat(depth*4));
        buf.push_str("let");
        if var_decl.peek_mutable() {
            buf.push_str(" mut");
        }
        buf.push_str(" ");
        buf.push_str(var_decl.peek_name());
        if let Some(t) = var_decl.peek_var_type() {
            Self::visit_type(&t, buf);
        }

        if let Some(v) = var_decl.peek_value() {
            buf.push_str(" = ");
            Self::visit_expr(&v, buf);
        }
        buf.push_str(";\n");
        Ok(())
    }

    fn visit_assignment(assignment: &Assignment, depth: usize, buf: &mut String) -> anyhow::Result<()> {
        buf.push_str(&" ".repeat(depth*4));
        buf.push_str(assignment.peek_name());
        buf.push_str(" = ");
        Self::visit_expr(assignment.peek_value(), buf);
        buf.push_str(";\n");
        Ok(())
    }

    fn visit_conditional(conditional: &Conditional, depth: usize, buf: &mut String) -> anyhow::Result<()> {
        buf.push_str(&" ".repeat(depth*4));
        buf.push_str("if ");
        Self::visit_expr(conditional.peek_condition(), buf)?;
        buf.push_str(" ");
        Self::visit_block(conditional.peek_if_block(), depth, buf)?;
        if let Some(e) = conditional.peek_else_block() {
            buf.push_str(" else ");
            Self::visit_block(e, depth, buf)?;
        }
        buf.push_str("\n");
        Ok(())
    }

    fn visit_repeat_loop(repeat_loop: &RepeatLoop, depth: usize, buf: &mut String) -> anyhow::Result<()> {
        buf.push_str(&" ".repeat(depth*4));
        buf.push_str("repeat ");
        Self::visit_block(repeat_loop.peek_body(), depth, buf)?;
        buf.push_str("\n");
        Ok(())
    }

    fn visit_while_loop(while_loop: &WhileLoop, depth: usize, buf: &mut String) -> anyhow::Result<()> {
        buf.push_str(&" ".repeat(depth*4));
        buf.push_str("while ");
        Self::visit_expr(while_loop.peek_condition(), buf)?;
        buf.push_str(" ");
        Self::visit_block(while_loop.peek_body(), depth, buf)?;
        buf.push_str("\n");
        Ok(())
    }

    fn visit_function_call(function_call: &FunctionCall, buf: &mut String) -> anyhow::Result<()> {
        buf.push_str(function_call.peek_name());
        buf.push_str("(");
        let mut args = function_call.peek_args().iter();
        if let Some(a) =  args.next() {
            Self::visit_expr(a, buf)?;
        }
        for arg in args {
            buf.push_str(", ");
            Self::visit_expr(arg, buf)?;
        }
        buf.push_str(")");
        Ok(())
    }

    fn visit_return(r: &Return, depth: usize, buf: &mut String) -> anyhow::Result<()> {
        buf.push_str(&" ".repeat(depth*4));
        buf.push_str("return ");
        Self::visit_expr(r.peek_value(), buf)?;
        buf.push_str(";\n");
        Ok(())
    }

    fn visit_expr(expr: &Expr, buf: &mut String) -> anyhow::Result<()> {
        match expr {
            Expr::Literal(l) => {Self::visit_literal(l, buf)?;},
            Expr::FunctionCall(f) => {Self::visit_function_call(f, buf)?;},
            Expr::Ident(s) => {buf.push_str(s);},
            Expr::UnaryOp(ops, e) => {Self::visit_unary_op(ops, *&e, buf)?;},
            Expr::BinaryOp(e1, op, e2) => {Self::visit_binary_op(*&e1, *op, *&e2, buf)?;},
        }
        Ok(())
    }

    fn visit_literal(literal: &Literal, buf: &mut String) -> anyhow::Result<()> {
        match literal {
            Literal::Int(u) => {buf.push_str(&u.to_string());},
            Literal::Str(s) => {
                buf.push_str("\"");
                buf.push_str(s);
                buf.push_str("\"");
            },
            Literal::Bool(b) => {buf.push_str(&b.to_string());},
        }
        Ok(())
    }

    fn visit_unary_op(ops: &Vec<UnaryOp>, expr: &Expr, buf: &mut String) -> anyhow::Result<()> {
        for op in ops {
            buf.push_str("-");
        }
        Self::visit_expr(expr, buf)?;
        Ok(())
    }

    fn visit_binary_op(first_expr: &Expr, op: BinaryOp, second_expr: &Expr, buf: &mut String) -> anyhow::Result<()> {
        Self::visit_expr(first_expr, buf)?;
        buf.push_str(" ");
        buf.push_str(op.into());
        buf.push_str(" ");
        Self::visit_expr(second_expr, buf)?;
        Ok(())
    }
}

pub trait AstVisitor {
    fn visit_module(module: &Module, buf: &mut String) -> anyhow::Result<()>;
    fn visit_func(func: &Func, buf: &mut String) -> anyhow::Result<()>;
    fn visit_param(param: &Param, buf: &mut String) -> anyhow::Result<()>;
    fn visit_type(t: &Type, buf: &mut String) -> anyhow::Result<()>;
    fn visit_block(block: &Block, depth: usize, buf: &mut String) -> anyhow::Result<()>;
    fn visit_stmt(stmt: &Statement, depth: usize, buf: &mut String) -> anyhow::Result<()>;
    fn visit_var_decl(var_decl: &VarDecl, depth: usize, buf: &mut String) -> anyhow::Result<()>;
    fn visit_assignment(assignment: &Assignment, depth: usize, buf: &mut String) -> anyhow::Result<()>;
    fn visit_conditional(conditional: &Conditional, depth: usize, buf: &mut String) -> anyhow::Result<()>;
    fn visit_repeat_loop(repeat_loop: &RepeatLoop, depth: usize, buf: &mut String) -> anyhow::Result<()>;
    fn visit_while_loop(while_loop: &WhileLoop, depth: usize, buf: &mut String) -> anyhow::Result<()>;
    fn visit_function_call(function_call: &FunctionCall, buf: &mut String) -> anyhow::Result<()>;
    fn visit_return(r: &Return, depth: usize, buf: &mut String) -> anyhow::Result<()>;
    fn visit_expr(expr: &Expr, buf: &mut String) -> anyhow::Result<()>;
    fn visit_literal(literal: &Literal, buf: &mut String) -> anyhow::Result<()>;
    fn visit_unary_op(ops: &Vec<UnaryOp>, expr: &Expr, buf: &mut String) -> anyhow::Result<()>;
    fn visit_binary_op(first_expr: &Expr, op: BinaryOp, second_expr: &Expr, buf: &mut String) -> anyhow::Result<()>;
}
