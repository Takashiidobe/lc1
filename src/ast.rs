use std::fmt::{self, Display};

use crate::codegen::Type;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value {
    Int(i64),
    Str(String),
}

impl From<i64> for Value {
    fn from(i: i64) -> Self {
        Value::Int(i)
    }
}

impl From<String> for Value {
    fn from(s: String) -> Self {
        Value::Str(s)
    }
}

impl From<&str> for Value {
    fn from(s: &str) -> Self {
        Value::Str(s.to_owned())
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(i) => f.write_str(&i.to_string()),
            Value::Str(s) => f.write_str(s),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum Expr {
    Add { lhs: Box<Expr>, rhs: Box<Expr> },
    Lt { lhs: Box<Expr>, rhs: Box<Expr> },
    Const { value: Value },
    Var { name: String },
    FnCall { name: String, args: Vec<Expr> },
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Add { lhs, rhs } => f.write_fmt(format_args!("{lhs} + {rhs}")),
            Expr::Lt { lhs, rhs } => f.write_fmt(format_args!("{lhs} < {rhs}")),
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
        args: Vec<(String, Type)>,
        body: Vec<Stmt>,
        return_type: Type,
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
