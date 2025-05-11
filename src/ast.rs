#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Hash)]
pub enum Expr {
    Add { lhs: Box<Expr>, rhs: Box<Expr> },
    Const { value: i64 },
    Var { name: String },
    FnCall { name: String, args: Vec<Expr> },
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
}
