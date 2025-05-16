use lc1::{
    ast::{Expr, Stmt, Value},
    codegen::Codegen,
};

use Expr::*;
use Stmt::*;

fn main() {
    let program = vec![
        FnDecl {
            name: "add".to_string(),
            args: vec![
                "a".into(),
                "b".into(),
                "c".into(),
                "d".into(),
                "e".into(),
                "f".into(),
                "g".into(),
            ],
            body: vec![Return {
                expr: Add {
                    lhs: Box::new(Var { name: "a".into() }),
                    rhs: Box::new(Add {
                        lhs: Box::new(Var { name: "b".into() }),
                        rhs: Box::new(Add {
                            lhs: Box::new(Var { name: "c".into() }),
                            rhs: Box::new(Add {
                                lhs: Box::new(Var { name: "d".into() }),
                                rhs: Box::new(Add {
                                    lhs: Box::new(Var { name: "e".into() }),
                                    rhs: Box::new(Add {
                                        lhs: Box::new(Var { name: "f".into() }),
                                        rhs: Box::new(Var { name: "g".into() }),
                                    }),
                                }),
                            }),
                        }),
                    }),
                },
            }],
        },
        VarDecl {
            name: "a".into(),
            value: Const {
                value: Value::Int(1),
            },
        },
        VarDecl {
            name: "b".into(),
            value: Const {
                value: Value::Int(1),
            },
        },
        Print {
            expr: FnCall {
                name: "add".into(),
                args: vec![
                    Var { name: "a".into() },
                    Var { name: "b".into() },
                    Var { name: "a".into() },
                    Var { name: "b".into() },
                    Var { name: "a".into() },
                    Var { name: "b".into() },
                    Var { name: "a".into() },
                ],
            },
        },
        Print {
            expr: Var { name: "b".into() },
        },
        If {
            cond: Const {
                value: Value::Int(1),
            },
            then: vec![Print {
                expr: Const {
                    value: Value::Int(10),
                },
            }],
            else_branch: Some(vec![Print {
                expr: Const {
                    value: Value::Int(20),
                },
            }]),
        },
        Return {
            expr: Add {
                lhs: Box::new(Var { name: "a".into() }),
                rhs: Box::new(Var { name: "b".into() }),
            },
        },
    ];

    let if_program = vec![If {
        cond: Const {
            value: Value::Int(0),
        },
        then: vec![Print {
            expr: Const {
                value: Value::Int(10),
            },
        }],
        else_branch: Some(vec![Print {
            expr: Const {
                value: Value::Int(20),
            },
        }]),
    }];

    let nested_if = vec![
        VarDecl {
            name: "x".into(),
            value: Const {
                value: Value::Int(1),
            },
        },
        If {
            cond: Const {
                value: Value::Int(1),
            },
            then: vec![
                VarDecl {
                    name: "x".into(),
                    value: Const {
                        value: Value::Int(2),
                    },
                },
                Print {
                    expr: Var { name: "x".into() },
                },
            ],
            else_branch: None,
        },
        Print {
            expr: Var { name: "x".into() },
        },
    ];

    let fibonacci_program = vec![
        FnDecl {
            name: "fibonacci".to_string(),
            args: vec!["n".into()],
            body: vec![If {
                cond: Lt {
                    lhs: Box::new(Var { name: "n".into() }),
                    rhs: Box::new(Const {
                        value: Value::Int(2),
                    }),
                },
                then: vec![Return {
                    expr: Var { name: "n".into() },
                }],
                else_branch: Some(vec![Return {
                    expr: Add {
                        lhs: Box::new(FnCall {
                            name: "fibonacci".into(),
                            args: vec![Add {
                                lhs: Box::new(Var { name: "n".into() }),
                                rhs: Box::new(Const {
                                    value: Value::Int(-1),
                                }),
                            }],
                        }),
                        rhs: Box::new(FnCall {
                            name: "fibonacci".into(),
                            args: vec![Add {
                                lhs: Box::new(Var { name: "n".into() }),
                                rhs: Box::new(Const {
                                    value: Value::Int(-2),
                                }),
                            }],
                        }),
                    },
                }]),
            }],
        },
        Print {
            expr: FnCall {
                name: "fibonacci".into(),
                args: vec![Const {
                    value: Value::Int(5),
                }],
            },
        },
    ];

    let hello_world = vec![Stmt::Print {
        expr: Expr::Const {
            value: Value::Str("hello, world!".into()),
        },
    }];

    let print_many = vec![
        Stmt::FnDecl {
            name: "print7".to_string(),
            args: vec![
                "a".into(),
                "b".into(),
                "c".into(),
                "d".into(),
                "e".into(),
                "f".into(),
                "g".into(),
            ],
            body: vec![
                Stmt::Print {
                    expr: Expr::Var { name: "a".into() },
                },
                Stmt::Print {
                    expr: Expr::Var { name: "b".into() },
                },
                Stmt::Print {
                    expr: Expr::Var { name: "c".into() },
                },
                Stmt::Print {
                    expr: Expr::Var { name: "d".into() },
                },
                Stmt::Print {
                    expr: Expr::Var { name: "e".into() },
                },
                Stmt::Print {
                    expr: Expr::Var { name: "f".into() },
                },
                Stmt::Print {
                    expr: Expr::Var { name: "g".into() },
                },
            ],
        },
        Stmt::Print {
            expr: Expr::FnCall {
                name: "print7".to_string(),
                args: vec![
                    Expr::Const {
                        value: Value::Str("one".into()),
                    },
                    Expr::Const {
                        value: Value::Str("two".into()),
                    },
                    Expr::Const {
                        value: Value::Str("three".into()),
                    },
                    Expr::Const {
                        value: Value::Str("four".into()),
                    },
                    Expr::Const {
                        value: Value::Str("five".into()),
                    },
                    Expr::Const {
                        value: Value::Str("six".into()),
                    },
                    Expr::Const {
                        value: Value::Str("seven".into()),
                    },
                ],
            },
        },
    ];

    let mut codegen = Codegen::default();
    println!("{}", codegen.run(&print_many));

    // let mut interpreter = Interpreter::default();
    // interpreter.run(&fibonacci_program);
}
