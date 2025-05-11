use std::fmt::{self, Display};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum Expr {
    Add { lhs: Box<Expr>, rhs: Box<Expr> },
    Const { value: i64 },
    Var { name: String },
    FnCall { name: String, args: Vec<Expr> },
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Add { lhs, rhs } => f.write_fmt(format_args!("{lhs} + {rhs}")),
            Expr::Const { value } => f.write_fmt(format_args!("{value}")),
            Expr::Var { name } => f.write_fmt(format_args!("{name}")),
            Expr::FnCall { name, args } => f.write_fmt(format_args!("{name}({args:?})")),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum Stmt {
    FnDecl {
        name: String,
        args: Vec<String>,
        body: Vec<Stmt>,
    },
    VarDecl {
        name: String,
        value: Expr,
    },
    Print {
        expr: Expr,
    },
    Return {
        expr: Expr,
    },
    If {
        cond: Expr,
        then: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },
}
