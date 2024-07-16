mod ast;

use anyhow::{anyhow, Result};

use crate::lexer::tokens::{Keyword, Token, Punctuation, Literal};
use crate::lexer::{tokenize, TokenStream};

pub fn parse(mut tokens: TokenStream<'_>) -> anyhow::Result<ast::Module> {
    Parser::new(tokens).parse_module()
}

struct Parser<'a> {
    tokens: TokenStream<'a>,
    current: Option<anyhow::Result<Token>>,
    next: Option<anyhow::Result<Token>>,
}

impl Parser<'_> {
    fn new(mut tokens: TokenStream<'_>) -> Parser<'_> {
        let current = tokens.next();
        let next = tokens.next();
        Parser {
            tokens,
            current,
            next,
        }
    }

    fn parse_module(&mut self) -> anyhow::Result<ast::Module> {
        let mut funcs = Vec::new();
        while !self.is_empty() {
            funcs.push(self.parse_func()?);
        }
        Ok(ast::Module { funcs })
    }

    fn parse_func(&mut self) -> anyhow::Result<ast::Func> {
        self.expect_keyword(Keyword::Func)?;
        let name = self.expect_ident()?;
        self.expect_punctuation(Punctuation::LeftParen)?;
        let mut param_list: Vec<ast::Param> = Vec::new();
        while self.has_param() {
            param_list.push(self.parse_param()?);
            if self.has_punctuation(Punctuation::Comma) {self.expect_punctuation(Punctuation::Comma)?;}
        }
        self.expect_punctuation(Punctuation::RightParen)?;
        let return_type = self.parse_type_annotation_if_present();
        let block = self.parse_block()?;
        Ok(ast::Func{name, param_list, return_type, block})
    }

    fn parse_param(&mut self) -> anyhow::Result<ast::Param> {
        let name = self.expect_ident()?;
        let param_type = self.parse_type_annotation()?;
        Ok(ast::Param{name, param_type})
    }

    fn parse_type_annotation(&mut self) -> anyhow::Result<ast::Type> {
        self.expect_punctuation(Punctuation::Colon)?;
        let name = self.expect_ident()?;
        Ok(ast::Type{name})
    }

    fn parse_type_annotation_if_present(&mut self) -> Option<ast::Type> {
        match self.has_punctuation(Punctuation::Colon) {
            true => match self.parse_type_annotation() {
                Ok(t) => Some(t),
                _ => None,
            },
            false => None,
        }
    }

    fn parse_block(&mut self) -> anyhow::Result<ast::Block> {
        self.expect_punctuation(Punctuation::LeftBrace)?;
        let mut stmts: Vec<ast::Statement> = Vec::new();
        while self.has_stmt() {
            stmts.push(self.parse_stmt()?);
        }
        self.expect_punctuation(Punctuation::RightBrace)?;
        Ok(ast::Block{stmts})
    }

    fn parse_stmt(&mut self) -> anyhow::Result<ast::Statement> {
        if self.has_var_decl() {
            Ok(ast::Statement::VarDecl(self.parse_var_decl()?))
        } else if self.has_conditional() {
            Ok(ast::Statement::Conditional(self.parse_conditional()?))
        } else if self.has_while_loop() {
            Ok(ast::Statement::WhileLoop(self.parse_while_loop()?))
        } else if self.has_repeat_loop() {
            Ok(ast::Statement::RepeatLoop(self.parse_repeat_loop()?))
        } else if self.has_break() {
            self.expect_break()?;
            self.expect_punctuation(Punctuation::Semi)?;
            Ok(ast::Statement::Break)
        } else if self.has_continue() {
            self.expect_continue()?;
            self.expect_punctuation(Punctuation::Semi)?;
            Ok(ast::Statement::Continue)
        } else if self.has_return() { 
            Ok(ast::Statement::Return(self.parse_return()?))
        } else if self.has_ident() {
            if self.has_punctuation_next(Punctuation::LeftParen) {
            println!("9");
                Ok(ast::Statement::FunctionCall(self.parse_func_call()?))
            } else if self.has_punctuation_next(Punctuation::Equals) {
                Ok(ast::Statement::Assignment(self.parse_assignment()?))
            } else {
                Err(anyhow!("expected assignment or function call"))
            }
        } else {
            Err(anyhow!("expected statement"))
        }
    }

    fn parse_var_decl(&mut self) -> anyhow::Result<ast::VarDecl> {
        self.expect_keyword(Keyword::Let)?;
        let mutable = self.has_keyword(Keyword::Mut);
        if mutable {self.consume_token();}
        let name = self.expect_ident()?;
        let var_type = self.parse_type_annotation_if_present();
        let value = match self.has_punctuation(Punctuation::Equals) {
            true => {
                self.expect_punctuation(Punctuation::Equals);
                self.parse_expr_if_present()
            },
            false => None,
        };
        self.expect_punctuation(Punctuation::Semi)?;
        Ok(ast::VarDecl{mutable, name, var_type, value})
    }

    fn parse_conditional(&mut self) -> anyhow::Result<ast::Conditional> {
        self.expect_keyword(Keyword::If)?;
        let condition = self.parse_expr()?;
        let if_block = self.parse_block()?;
        let else_block = match self.has_keyword(Keyword::Else) {
            true => {
                self.expect_keyword(Keyword::Else)?;
                Some(self.parse_block()?)
            },
            false => None,
        };
        Ok(ast::Conditional{condition, if_block, else_block})
    }

    fn parse_while_loop(&mut self) -> anyhow::Result<ast::WhileLoop> {
        self.expect_keyword(Keyword::While)?;
        let condition = self.parse_expr()?;
        let body = self.parse_block()?;
        Ok(ast::WhileLoop{condition, body})
    }

    fn parse_repeat_loop(&mut self) -> anyhow::Result<ast::RepeatLoop> {
        self.expect_keyword(Keyword::Repeat)?;
        let body = self.parse_block()?;
        Ok(ast::RepeatLoop{body})
    }

    fn parse_return(&mut self) -> anyhow::Result<ast::Return> {
        self.expect_keyword(Keyword::Return)?;
        let value = self.parse_expr()?;
        self.expect_punctuation(Punctuation::Semi)?;
        Ok(ast::Return{value})
    }

    fn parse_assignment(&mut self) -> anyhow::Result<ast::Assignment> {
        let name = self.expect_ident()?;
        self.expect_punctuation(Punctuation::Equals)?;
        let value = self.parse_expr()?;
        self.expect_punctuation(Punctuation::Semi)?;
        Ok(ast::Assignment{name, value})
    }

    fn parse_expr(&mut self) -> anyhow::Result<ast::Expr> {
        self.parse_rel_expr()    
    }

    fn parse_expr_if_present(&mut self) -> Option<ast::Expr> {
        match self.parse_expr() {
            Ok(e) => Some(e),
            _ => None,
        }
    }

    fn parse_rel_expr(&mut self) -> anyhow::Result<ast::Expr> {
        let mut expr = self.parse_term_expr()?;
        while self.has_rel_op() {
            expr = ast::Expr::BinaryOp(Box::new(expr), self.parse_rel_op()?, Box::new(self.parse_term_expr()?));
        }
        Ok(expr)
    }

    fn parse_term_expr(&mut self) -> anyhow::Result<ast::Expr> {
        let mut expr = self.parse_factor_expr()?;
        while self.has_term_op() {
            expr = ast::Expr::BinaryOp(Box::new(expr), self.parse_term_op()?, Box::new(self.parse_factor_expr()?));
        }
        Ok(expr)
    }

    fn parse_factor_expr(&mut self) -> anyhow::Result<ast::Expr> {
        let mut expr = self.parse_base_expr()?;
        while self.has_factor_op() {
            expr = ast::Expr::BinaryOp(Box::new(expr), self.parse_factor_op()?, Box::new(self.parse_base_expr()?));
        }
        Ok(expr)
    }

    fn parse_base_expr(&mut self) -> anyhow::Result<ast::Expr> {
        let mut unary_ops = Vec::new();
        while self.has_unary_op() {
            unary_ops.push(self.parse_unary_op()?);
        }

        let expr = if self.has_literal() {
            ast::Expr::Literal(self.expect_literal()?)
        } else if self.has_ident() {
            if self.has_punctuation_next(Punctuation::LeftParen) {
                ast::Expr::FunctionCall(self.parse_func_call()?)
            } else {
                ast::Expr::Ident(self.expect_ident()?)
            }
        } else if self.has_punctuation(Punctuation::LeftParen) {
            self.parse_expr()?
        } else {
            return Err(anyhow!("no expression found"));
        };

        if unary_ops.is_empty() {
            Ok(expr)
        } else {
            Ok(ast::Expr::UnaryOp(unary_ops, Box::new(expr)))
        }
    }

    fn parse_func_call(&mut self) -> anyhow::Result<ast::FunctionCall> {
        let mut args: Vec<ast::Expr> = Vec::new();
        let name = self.expect_ident()?;
        self.expect_punctuation(Punctuation::LeftParen)?;
        while self.has_expr() {
            args.push(self.parse_expr()?);
        }
        self.expect_punctuation(Punctuation::RightParen)?;
        Ok(ast::FunctionCall{name, args})
    }

    fn parse_rel_op(&mut self) -> anyhow::Result<ast::BinaryOp> {
        match self.consume_token()? {
            Token::Punctuation(Punctuation::LeftAngle) => match self.has_punctuation(Punctuation::Equals) {
                true => {
                    self.consume_token();
                    Ok(ast::BinaryOp::Le)
                },
                false => Ok(ast::BinaryOp::Lt),
            },
            Token::Punctuation(Punctuation::RightAngle) => match self.has_punctuation(Punctuation::Equals) {
                true => {
                    self.consume_token();
                    Ok(ast::BinaryOp::Ge)
                },
                false => Ok(ast::BinaryOp::Gt),
            },
            Token::Punctuation(Punctuation::Equals) => match self.has_punctuation(Punctuation::Equals) {
                true => {
                    self.consume_token();
                    Ok(ast::BinaryOp::Eq)
                },
                false => Err(anyhow!("expected ==")),
            },
            Token::Punctuation(Punctuation::Bang) => match self.has_punctuation(Punctuation::Equals) {
                true => {
                    self.consume_token();
                    Ok(ast::BinaryOp::Ne)
                },
                false => Err(anyhow!("expected !=")),
            },
            _ => Err(anyhow!("expected relational operator")),
        }
    }

    fn parse_term_op(&mut self) -> anyhow::Result<ast::BinaryOp> {
        match self.consume_token()? {
            Token::Punctuation(Punctuation::Plus) => Ok(ast::BinaryOp::Plus),
            Token::Punctuation(Punctuation::Minus) => Ok(ast::BinaryOp::Minus),
            _ => Err(anyhow!("expected term operator")),
        }
    }

    fn parse_factor_op(&mut self) -> anyhow::Result<ast::BinaryOp> {
        match self.consume_token()? {
            Token::Punctuation(Punctuation::Star) => Ok(ast::BinaryOp::Mul),
            Token::Punctuation(Punctuation::Div) => Ok(ast::BinaryOp::Div),
            _ => Err(anyhow!("expected factor operator")),
        }
    }

    fn parse_unary_op(&mut self) -> anyhow::Result<ast::UnaryOp> {
        match self.consume_token()? {
            Token::Punctuation(Punctuation::Minus) => Ok(ast::UnaryOp::Neg),
            _ => Err(anyhow!("expect unary operator")),
        }
    }

    fn has_unary_op(&self) -> bool {
        matches!(self.peek(), Some(&Token::Punctuation(Punctuation::Minus)))
    }

    fn has_rel_op(&self) -> bool {
        matches!(self.peek(), Some(&Token::Punctuation(Punctuation::Equals))) && matches!(self.peek_next(), Some(&Token::Punctuation(Punctuation::Equals)))
        || matches!(self.peek(), Some(&Token::Punctuation(Punctuation::Bang))) && matches!(self.peek_next(), Some(&Token::Punctuation(Punctuation::Equals)))
        || matches!(self.peek(), Some(&Token::Punctuation(Punctuation::LeftAngle))) && matches!(self.peek_next(), Some(&Token::Punctuation(Punctuation::Equals)))
        || matches!(self.peek(), Some(&Token::Punctuation(Punctuation::RightAngle))) && matches!(self.peek_next(), Some(&Token::Punctuation(Punctuation::Equals)))
        || matches!(self.peek(), Some(&Token::Punctuation(Punctuation::LeftAngle)))
        || matches!(self.peek(), Some(&Token::Punctuation(Punctuation::RightAngle)))
    }

    fn has_term_op(&self) -> bool {
        matches!(self.peek(), Some(&Token::Punctuation(Punctuation::Minus))) 
        || matches!(self.peek(), Some(&Token::Punctuation(Punctuation::Plus)))
    }

    fn has_factor_op(&self) -> bool {
        matches!(self.peek(), Some(&Token::Punctuation(Punctuation::Star))) 
        || matches!(self.peek(), Some(&Token::Punctuation(Punctuation::Div)))
    }

    fn has_literal(&self) -> bool {
        matches!(self.peek(), Some(&Token::Literal(_)))
    }

    fn has_ident(&self) -> bool {
        matches!(self.peek(), Some(&Token::Ident(_)))
    }

    fn has_punctuation(&self, target: Punctuation) -> bool {
        matches!(self.peek(), Some(&Token::Punctuation(p)) if p == target) 
    }

    fn has_punctuation_next(&self, target: Punctuation) -> bool {
        matches!(self.peek_next(), Some(&Token::Punctuation(p))if p == target)
    }

    fn has_expr(&self) -> bool {
        self.has_unary_op()
        || self.has_literal()
        || self.has_ident()
        || self.has_punctuation(Punctuation::LeftParen)
    }

    fn has_param(&self) -> bool {
        self.has_ident() && self.has_punctuation_next(Punctuation::Colon)
    }

    fn has_stmt(&self) -> bool {
        self.has_ident() 
        || self.has_keyword(Keyword::Continue)
        || self.has_keyword(Keyword::Break)
        || self.has_keyword(Keyword::If)
        || self.has_keyword(Keyword::Else)
        || self.has_keyword(Keyword::While)
        || self.has_keyword(Keyword::Repeat)
        || self.has_keyword(Keyword::Let)
        || self.has_keyword(Keyword::Return)
    }

    fn has_keyword(&self, target: Keyword) -> bool {
        matches!(self.peek(), Some(&Token::Keyword(k)) if k == target)
    }

    fn has_var_decl(&self) -> bool {
        self.has_keyword(Keyword::Let)
    }

    fn has_conditional(&self) -> bool {
        self.has_keyword(Keyword::If)
    }

    fn has_while_loop(&self) -> bool {
        self.has_keyword(Keyword::While)
    }

    fn has_repeat_loop(&self) -> bool {
        self.has_keyword(Keyword::Repeat)
    }

    fn has_break(&self) -> bool {
        self.has_keyword(Keyword::Break)
    }

    fn has_continue(&self) -> bool {
        self.has_keyword(Keyword::Continue)
    }

    fn has_return(&self) -> bool {
        self.has_keyword(Keyword::Return)
    }

    fn expect_break(&mut self) -> anyhow::Result<()> {
        match self.consume_token()? {
            Token::Keyword(Keyword::Break) => Ok(()),
            _ => Err(anyhow!("expexted break keyword")),
        }
    }

    fn expect_continue(&mut self) -> anyhow::Result<()> {
        match self.consume_token()? {
            Token::Keyword(Keyword::Continue) => Ok(()),
            _ => Err(anyhow!("expected continue keyword")),
        }
    }

    fn expect_punctuation(&mut self, target: Punctuation) -> anyhow::Result<()> {
        println!("expected {} found  {}",target, self.peek().unwrap());
        match self.consume_token()? {
            Token::Punctuation(p) if p == target => {
                Ok(())
            },
            other => Err(anyhow!("expected punctuaion: {} found {}", target, other)),
        }
    }

    fn expect_literal(&mut self) -> anyhow::Result<Literal> {
        match self.consume_token()? {
            Token::Literal(l) => Ok(l),
            _ => Err(anyhow!("expected literal")),
        }
    }

    fn expect_ident(&mut self) -> anyhow::Result<String> {
        match self.consume_token()? {
            Token::Ident(i) => {
                Ok(i)
            },
            _ => Err(anyhow!("expected identifier")),
        }
    }

    fn expect_keyword(&mut self, target: Keyword) -> anyhow::Result<()> {
        match self.consume_token()? {
            Token::Keyword(kw) if kw == target => {
                Ok(())
            },
            _ => Err(anyhow!("expected {}", target)),
        }
    }
    
    fn consume_token(&mut self) -> anyhow::Result<Token> {
        println!("{}", self.peek().unwrap());
        let current = self.current.take();
        self.current = self.next.take();
        self.next = self.tokens.next();
        current.unwrap_or(Err(anyhow!("no remaining tokens")))
    }

    fn is_empty(&mut self) -> bool {
        match self.peek() {
            Some(_) => false,
            None => true,
        }
    }

    fn peek(&self) -> Option<&Token> {
        match self.current.as_ref() {
            Some(r) => match r {
                Ok(t) => Some(t),
                Err(_) => None,
            },
            None => None,
        }
    }

    fn peek_next(&self) -> Option<&Token> {
        match self.next.as_ref() {
            Some(r) => match r {
                Ok(t) => Some(t),
                Err(_) => None,
            },
            None => None,
        }
    }
}

mod tests {
    use crate::parser::*;
    use crate::lexer::*;
    
    #[test]
    fn test_func() {
        let funcs = "func test_func(first_param: first_type, second_param: second_type): return_type {} 
                     func another_func() {}";
        let test_module = format!("{:?}", parse(tokenize(funcs)).expect("error"));
        let result = "Module { funcs: [Func { name: \"test_func\", param_list: [Param { name: \"first_param\", param_type: Type { name: \"first_type\" } }, Param { name: \"second_param\", param_type: Type { name: \"second_type\" } }], return_type: Some(Type { name: \"return_type\" }), block: Block { stmts: [] } }, Func { name: \"another_func\", param_list: [], return_type: None, block: Block { stmts: [] } }] }";
        assert_eq!(result, test_module);
    }
    
    #[test]
    fn test_assignment() {
        let func = "func test() {
            let x: int = 1;
            let mut y = \"hello\";
            let z;
        }";
        let test_module = format!("{:?}", parse(tokenize(func)).expect("error"));
        let result = "Module { funcs: [Func { name: \"test\", param_list: [], return_type: None, block: Block { stmts: [VarDecl(VarDecl { mutable: false, name: \"x\", var_type: Some(Type { name: \"int\" }), value: Some(Literal(Int(1))) }), VarDecl(VarDecl { mutable: true, name: \"y\", var_type: None, value: Some(Literal(Str(\"hello\"))) }), VarDecl(VarDecl { mutable: false, name: \"z\", var_type: None, value: None })] } }] }";
        assert_eq!(result, test_module);
    }

    #[test]
    fn test_data_types() {
        let func = "func test() {
            let i: int = 1000;
            let s: String = \"Hello World\";
            let b: bool = true;
        }";
        let test_module = parse(tokenize(func));
        println!("{:?}", test_module.expect("error"));
    }

    #[test]
    fn test_operators() {
        let func = "func test() {
            let x = 1 + 2;
            let x = 2 - 1;
            let x = 2 * 2;
            let x = 4 / 2;
            let x = true == true;
            let x = false != true;
            let x = 10 < 9;
            let x = 9 > 10;
            let x = 10 <= 9;
            let x = 9 >= 10;
            let x = -21;
        }";
        let test_module = format!("{:?}", parse(tokenize(func)).expect("error"));
        let result = "Module { funcs: [Func { name: \"test\", param_list: [], return_type: None, block: Block { stmts: [VarDecl(VarDecl { mutable: false, name: \"x\", var_type: None, value: Some(BinaryOp(Literal(Int(1)), Plus, Literal(Int(2)))) }), VarDecl(VarDecl { mutable: false, name: \"x\", var_type: None, value: Some(BinaryOp(Literal(Int(2)), Minus, Literal(Int(1)))) }), VarDecl(VarDecl { mutable: false, name: \"x\", var_type: None, value: Some(BinaryOp(Literal(Int(2)), Mul, Literal(Int(2)))) }), VarDecl(VarDecl { mutable: false, name: \"x\", var_type: None, value: Some(BinaryOp(Literal(Int(4)), Div, Literal(Int(2)))) }), VarDecl(VarDecl { mutable: false, name: \"x\", var_type: None, value: Some(BinaryOp(Literal(Bool(true)), Eq, Literal(Bool(true)))) }), VarDecl(VarDecl { mutable: false, name: \"x\", var_type: None, value: Some(BinaryOp(Literal(Bool(false)), Ne, Literal(Bool(true)))) }), VarDecl(VarDecl { mutable: false, name: \"x\", var_type: None, value: Some(BinaryOp(Literal(Int(10)), Lt, Literal(Int(9)))) }), VarDecl(VarDecl { mutable: false, name: \"x\", var_type: None, value: Some(BinaryOp(Literal(Int(9)), Gt, Literal(Int(10)))) }), VarDecl(VarDecl { mutable: false, name: \"x\", var_type: None, value: Some(BinaryOp(Literal(Int(10)), Le, Literal(Int(9)))) }), VarDecl(VarDecl { mutable: false, name: \"x\", var_type: None, value: Some(BinaryOp(Literal(Int(9)), Ge, Literal(Int(10)))) }), VarDecl(VarDecl { mutable: false, name: \"x\", var_type: None, value: Some(UnaryOp([Neg], Literal(Int(21)))) })] } }] }";
        assert_eq!(result, test_module);
    }
    
    #[test]
    fn test_control_flow() {
        let func = "func test() {
            if true {
                
            } 
            
            if true {

            } else {

            }

            while true {
            
            }
        }";
        let test_module = format!("{:?}", parse(tokenize(func)).expect("error"));
        let result = "Module { funcs: [Func { name: \"test\", param_list: [], return_type: None, block: Block { stmts: [Conditional(Conditional { condition: Literal(Bool(true)), if_block: Block { stmts: [] }, else_block: None }), Conditional(Conditional { condition: Literal(Bool(true)), if_block: Block { stmts: [] }, else_block: Some(Block { stmts: [] }) }), WhileLoop(WhileLoop { condition: Literal(Bool(true)), body: Block { stmts: [] } })] } }] }";
        assert_eq!(result, test_module);
    }
    
    #[test]
    fn test_all() {
        let func = "func test() {
            let x: int = 512;
            let y: int = 256;
            if x / y == 2 {
                return \"x / y = 2\";
            } else {
                let y = 500;
            }

            while x > y {
                let y = y + 1;
            }
            
            repeat {
                let x = 2 * x;
            }

            return \"hello\";
        }";
        let test_module = format!("{:?}", parse(tokenize(func)).expect("error"));
        let result = "Module { funcs: [Func { name: \"test\", param_list: [], return_type: None, block: Block { stmts: [VarDecl(VarDecl { mutable: false, name: \"x\", var_type: Some(Type { name: \"int\" }), value: Some(Literal(Int(512))) }), VarDecl(VarDecl { mutable: false, name: \"y\", var_type: Some(Type { name: \"int\" }), value: Some(Literal(Int(256))) }), Conditional(Conditional { condition: BinaryOp(BinaryOp(Ident(\"x\"), Div, Ident(\"y\")), Eq, Literal(Int(2))), if_block: Block { stmts: [Return(Return { value: Literal(Str(\"x / y = 2\")) })] }, else_block: Some(Block { stmts: [VarDecl(VarDecl { mutable: false, name: \"y\", var_type: None, value: Some(Literal(Int(500))) })] }) }), WhileLoop(WhileLoop { condition: BinaryOp(Ident(\"x\"), Gt, Ident(\"y\")), body: Block { stmts: [VarDecl(VarDecl { mutable: false, name: \"y\", var_type: None, value: Some(BinaryOp(Ident(\"y\"), Plus, Literal(Int(1)))) })] } }), RepeatLoop(RepeatLoop { body: Block { stmts: [VarDecl(VarDecl { mutable: false, name: \"x\", var_type: None, value: Some(BinaryOp(Literal(Int(2)), Mul, Ident(\"x\"))) })] } }), Return(Return { value: Literal(Str(\"hello\")) })] } }] }";
        assert_eq!(result, test_module);
    }
}
