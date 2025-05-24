use std::{
    collections::BTreeMap,
    fmt::{self, Display},
};

use crate::codegen::Type;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Value {
    Int(i64),
    Str(String),
    Array(Vec<Value>),
    Struct(String, BTreeMap<String, Value>),
    Null,
}

impl Value {
    pub fn to_int(&self) -> i64 {
        match self {
            Value::Int(i) => *i,
            _ => panic!("Expected integer, got {:?}", self),
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Null => false,
            Value::Int(n) => *n != 0,
            Value::Str(s) => !s.is_empty(),
            Value::Array(..) | Value::Struct(..) => true,
        }
    }
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
            Value::Str(s) => f.write_str(&format!("\"{}\"", s)),
            Value::Null => f.write_str("null"),
            Value::Array(items) => {
                write!(f, "[")?;
                for (i, v) in items.iter().enumerate() {
                    write!(f, "{}", v)?;
                    if i + 1 < items.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Value::Struct(name, fields) => {
                write!(f, "struct {name} {{")?;
                for (i, (field_name, field_val)) in fields.iter().enumerate() {
                    write!(f, "{field_name}: {field_val}")?;
                    if i + 1 < fields.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum Expr {
    Neg {
        expr: Box<Expr>,
    },
    Add {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Sub {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Mul {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Div {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Lt {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Le {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Gt {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Ge {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Eq {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Ne {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Array {
        items: Vec<Expr>,
    },
    Const {
        value: Value,
    },
    Var {
        name: String,
    },
    FnCall {
        name: String,
        args: Vec<Expr>,
    },
    Index {
        array: Box<Expr>,
        index: Box<Expr>,
    },
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
    },
    Struct {
        name: String,
        fields: Vec<(String, Expr)>,
    },
    Field {
        object: Box<Expr>,
        name: String,
    },
}

impl Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Neg { expr } => f.write_fmt(format_args!("-{expr}")),
            Expr::Add { lhs, rhs } => f.write_fmt(format_args!("{lhs} + {rhs}")),
            Expr::Sub { lhs, rhs } => f.write_fmt(format_args!("{lhs} - {rhs}")),
            Expr::Mul { lhs, rhs } => f.write_fmt(format_args!("{lhs} * {rhs}")),
            Expr::Div { lhs, rhs } => f.write_fmt(format_args!("{lhs} / {rhs}")),
            Expr::Lt { lhs, rhs } => f.write_fmt(format_args!("{lhs} < {rhs}")),
            Expr::Le { lhs, rhs } => f.write_fmt(format_args!("{lhs} <= {rhs}")),
            Expr::Gt { lhs, rhs } => f.write_fmt(format_args!("{lhs} > {rhs}")),
            Expr::Ge { lhs, rhs } => f.write_fmt(format_args!("{lhs} >= {rhs}")),
            Expr::Eq { lhs, rhs } => f.write_fmt(format_args!("{lhs} == {rhs}")),
            Expr::Ne { lhs, rhs } => f.write_fmt(format_args!("{lhs} != {rhs}")),
            Expr::Const { value } => f.write_fmt(format_args!("{value}")),
            Expr::Var { name } => f.write_fmt(format_args!("{name}")),
            Expr::FnCall { name, args } => f.write_fmt(format_args!("{name}({args:?})")),
            Expr::Array { items } => {
                write!(f, "[")?;
                for (i, v) in items.iter().enumerate() {
                    write!(f, "{}", v)?;
                    if i + 1 < items.len() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "]")
            }
            Expr::Index { array, index } => f.write_fmt(format_args!("{array}[{index}]")),
            Expr::Assign { target, value } => f.write_fmt(format_args!("{target} = {value}")),
            Expr::Struct { name, fields } => {
                write!(f, "struct {name} {{")?;
                for (i, (name, expr)) in fields.iter().enumerate() {
                    write!(f, "{name}: {expr}")?;
                    if i + 1 < fields.len() {
                        write!(f, ",")?;
                        writeln!(f)?;
                    }
                }
                write!(f, "}}")
            }
            Expr::Field { object, name } => f.write_fmt(format_args!("{object}.{name}")),
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
    StructDecl {
        name: String,
        fields: Vec<(String, Type)>,
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
    Expr {
        expr: Expr,
    },
    For {
        init: Option<Box<Stmt>>,
        cond: Expr,
        post: Option<Box<Expr>>,
        body: Vec<Stmt>,
    },
}
