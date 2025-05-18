use crate::{
    ast::{Expr, Stmt, Value},
    codegen::Type,
    tokenizer::Token,
};

pub struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.pos).unwrap_or(&Token::Eof)
    }

    fn next(&mut self) -> Token {
        let t = self.peek().clone();
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        t
    }

    fn expect(&mut self, want: &Token) {
        let got = self.next();
        if &got != want {
            panic!("Expected {:?}, got {:?}", want, got);
        }
    }

    fn consume(&mut self, want: &Token) -> bool {
        if self.peek() == want {
            self.pos += 1;
            true
        } else {
            false
        }
    }

    pub fn parse_program(&mut self) -> Vec<Stmt> {
        let mut stmts = Vec::new();
        while self.peek() != &Token::Eof {
            stmts.push(self.parse_stmt());
        }
        stmts
    }

    fn parse_stmt(&mut self) -> Stmt {
        match self.peek() {
            Token::Fn => self.parse_fn_decl(),
            Token::Let => self.parse_var_decl(),
            Token::Print => self.parse_print(),
            Token::Return => self.parse_return(),
            Token::If => self.parse_if(),
            other => panic!("Unexpected token in statement position: {:?}", other),
        }
    }

    fn parse_fn_decl(&mut self) -> Stmt {
        self.expect(&Token::Fn);
        let name = if let Token::Ident(n) = self.next() {
            n
        } else {
            panic!("Expected identifier after `fn`");
        };
        self.expect(&Token::LParen);

        // parse ( name: Type, ... )
        let mut args = Vec::new();
        if !self.consume(&Token::RParen) {
            loop {
                let arg_name = if let Token::Ident(n) = self.next() {
                    n
                } else {
                    panic!("Expected ident in fn args");
                };
                self.expect(&Token::Colon);
                let ty = self.parse_type();
                args.push((arg_name, ty));
                if self.consume(&Token::RParen) {
                    break;
                }
                self.expect(&Token::Comma);
            }
        }

        // optional return arrow
        let return_type = if self.consume(&Token::Arrow) {
            self.parse_type()
        } else {
            Type::Null
        };

        // body
        self.expect(&Token::LBrace);
        let mut body = Vec::new();
        while !self.consume(&Token::RBrace) {
            body.push(self.parse_stmt());
        }

        Stmt::FnDecl {
            name,
            args,
            body,
            return_type,
        }
    }

    fn parse_var_decl(&mut self) -> Stmt {
        self.expect(&Token::Let);
        let name = if let Token::Ident(n) = self.next() {
            n
        } else {
            panic!("Expected identifier after `let`");
        };
        self.expect(&Token::Assign);
        let value = self.parse_expr();
        self.expect(&Token::Semi);
        Stmt::VarDecl { name, value }
    }

    fn parse_print(&mut self) -> Stmt {
        self.expect(&Token::Print);
        self.expect(&Token::LParen);
        let expr = self.parse_expr();
        self.expect(&Token::RParen);
        self.expect(&Token::Semi);
        Stmt::Print { expr }
    }

    fn parse_return(&mut self) -> Stmt {
        self.expect(&Token::Return);
        let expr = self.parse_expr();
        self.expect(&Token::Semi);
        Stmt::Return { expr }
    }

    fn parse_if(&mut self) -> Stmt {
        self.expect(&Token::If);
        self.expect(&Token::LParen);
        let cond = self.parse_expr();
        self.expect(&Token::RParen);

        // then‐block
        self.expect(&Token::LBrace);
        let mut then = Vec::new();
        while !self.consume(&Token::RBrace) {
            then.push(self.parse_stmt());
        }

        // optional else
        let else_branch = if self.consume(&Token::Else) {
            self.expect(&Token::LBrace);
            let mut els = Vec::new();
            while !self.consume(&Token::RBrace) {
                els.push(self.parse_stmt());
            }
            Some(els)
        } else {
            None
        };

        Stmt::If {
            cond,
            then,
            else_branch,
        }
    }

    fn parse_type(&mut self) -> Type {
        if let Token::Ident(t) = self.next() {
            match t.as_str() {
                "int" => Type::Int,
                "str" => Type::Str,
                "()" => Type::Null,
                _ => panic!("Unknown type: {}", t),
            }
        } else {
            panic!("Expected type name, got {:?}", self.peek());
        }
    }

    fn parse_expr(&mut self) -> Expr {
        self.parse_eq()
    }

    fn parse_eq(&mut self) -> Expr {
        let mut left = self.parse_rel();
        loop {
            if self.consume(&Token::Eq) {
                let right = self.parse_rel();
                left = Expr::Eq {
                    lhs: Box::new(left),
                    rhs: Box::new(right),
                };
            } else if self.consume(&Token::Ne) {
                let right = self.parse_rel();
                left = Expr::Ne {
                    lhs: Box::new(left),
                    rhs: Box::new(right),
                };
            } else {
                break;
            }
        }
        left
    }

    fn parse_rel(&mut self) -> Expr {
        let mut left = self.parse_add();
        loop {
            if self.consume(&Token::Lt) {
                let right = self.parse_add();
                left = Expr::Lt {
                    lhs: Box::new(left),
                    rhs: Box::new(right),
                };
            } else if self.consume(&Token::Le) {
                let right = self.parse_add();
                left = Expr::Le {
                    lhs: Box::new(left),
                    rhs: Box::new(right),
                };
            } else if self.consume(&Token::Gt) {
                let right = self.parse_add();
                left = Expr::Gt {
                    lhs: Box::new(left),
                    rhs: Box::new(right),
                };
            } else if self.consume(&Token::Ge) {
                let right = self.parse_add();
                left = Expr::Ge {
                    lhs: Box::new(left),
                    rhs: Box::new(right),
                };
            } else {
                break;
            }
        }
        left
    }

    fn parse_add(&mut self) -> Expr {
        let mut left = self.parse_unary();
        while self.consume(&Token::Plus) {
            let right = self.parse_unary();
            left = Expr::Add {
                lhs: Box::new(left),
                rhs: Box::new(right),
            };
        }
        left
    }

    fn parse_unary(&mut self) -> Expr {
        if self.consume(&Token::Minus) {
            let inner = self.parse_unary();
            Expr::Neg {
                expr: Box::new(inner),
            }
        } else {
            self.parse_postfix()
        }
    }

    fn parse_postfix(&mut self) -> Expr {
        let mut expr = self.parse_primary();

        while self.consume(&Token::LBracket) {
            let index_expr = self.parse_expr();
            self.expect(&Token::RBracket);
            expr = Expr::Index {
                array: Box::new(expr),
                index: Box::new(index_expr),
            };
        }

        expr
    }

    fn parse_primary(&mut self) -> Expr {
        self.process_primary()
    }

    fn process_primary(&mut self) -> Expr {
        match self.peek().clone() {
            Token::LBracket => {
                self.next();
                let mut items = vec![];
                if !self.consume(&Token::RBracket) {
                    loop {
                        items.push(self.parse_expr());
                        if self.consume(&Token::RBracket) {
                            break;
                        }
                        self.expect(&Token::Comma);
                    }
                }
                Expr::Array { items }
            }
            Token::Int(n) => {
                self.next();
                Expr::Const {
                    value: Value::Int(n),
                }
            }
            Token::Str(s) => {
                self.next();
                Expr::Const {
                    value: Value::Str(s),
                }
            }
            Token::Ident(name) => {
                self.next();
                if self.consume(&Token::LParen) {
                    let mut args = Vec::new();
                    if !self.consume(&Token::RParen) {
                        loop {
                            args.push(self.parse_expr());
                            if self.consume(&Token::RParen) {
                                break;
                            }
                            self.expect(&Token::Comma);
                        }
                    }
                    Expr::FnCall { name, args }
                } else {
                    Expr::Var { name }
                }
            }
            Token::LParen => {
                self.next();
                let e = self.parse_expr();
                self.expect(&Token::RParen);
                e
            }
            other => panic!("Unexpected token in expression: {:?}", other),
        }
    }
}
