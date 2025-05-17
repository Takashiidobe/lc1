use std::fmt::{self, Display};

use crate::codegen::Type;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value {
    Int(i64),
    Str(String),
    Null,
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
            Value::Null => f.write_str("null"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum Expr {
    Neg { expr: Box<Expr> },
    Add { lhs: Box<Expr>, rhs: Box<Expr> },
    Lt { lhs: Box<Expr>, rhs: Box<Expr> },
    Le { lhs: Box<Expr>, rhs: Box<Expr> },
    Gt { lhs: Box<Expr>, rhs: Box<Expr> },
    Ge { lhs: Box<Expr>, rhs: Box<Expr> },
    Eq { lhs: Box<Expr>, rhs: Box<Expr> },
    Ne { lhs: Box<Expr>, rhs: Box<Expr> },
    Const { value: Value },
    Var { name: String },
    FnCall { name: String, args: Vec<Expr> },
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Neg { expr } => f.write_fmt(format_args!("-{expr}")),
            Expr::Add { lhs, rhs } => f.write_fmt(format_args!("{lhs} + {rhs}")),
            Expr::Lt { lhs, rhs } => f.write_fmt(format_args!("{lhs} < {rhs}")),
            Expr::Le { lhs, rhs } => f.write_fmt(format_args!("{lhs} <= {rhs}")),
            Expr::Gt { lhs, rhs } => f.write_fmt(format_args!("{lhs} > {rhs}")),
            Expr::Ge { lhs, rhs } => f.write_fmt(format_args!("{lhs} >= {rhs}")),
            Expr::Eq { lhs, rhs } => f.write_fmt(format_args!("{lhs} == {rhs}")),
            Expr::Ne { lhs, rhs } => f.write_fmt(format_args!("{lhs} != {rhs}")),
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
