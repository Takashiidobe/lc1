use lc1::{
    ast::{Expr, Stmt, Value},
    codegen::{Codegen, Type},
};

use Expr::*;
use Stmt::*;

fn main() {
    let program = vec![
        FnDecl {
            name: "add".to_string(),
            args: vec![
                ("a".into(), Type::Int),
                ("b".into(), Type::Int),
                ("c".into(), Type::Int),
                ("d".into(), Type::Int),
                ("e".into(), Type::Int),
                ("f".into(), Type::Int),
                ("g".into(), Type::Int),
            ],
            return_type: Type::Int,
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

    // A simple if‐program (no FnDecl here, so it stays the same)
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

    // Nested‐if with shadowed variable
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

    // Fibonacci function, annotated as returning Int
    let fibonacci_program = vec![
        FnDecl {
            name: "fibonacci".to_string(),
            args: vec![("n".into(), Type::Int)],
            return_type: Type::Int,
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

    // Hello world (no FnDecl here)
    let hello_world = vec![Print {
        expr: Const {
            value: Value::Str("hello, world!".into()),
        },
    }];

    // print7 now has seven String arguments, each annotated Type::Str
    let print_many = vec![
        FnDecl {
            name: "print7".to_string(),
            args: vec![
                ("a".into(), Type::Str),
                ("b".into(), Type::Str),
                ("c".into(), Type::Str),
                ("d".into(), Type::Str),
                ("e".into(), Type::Str),
                ("f".into(), Type::Str),
                ("g".into(), Type::Str),
            ],
            return_type: Type::Null,
            body: vec![
                Print {
                    expr: Var { name: "a".into() },
                },
                Print {
                    expr: Var { name: "b".into() },
                },
                Print {
                    expr: Var { name: "c".into() },
                },
                Print {
                    expr: Var { name: "d".into() },
                },
                Print {
                    expr: Var { name: "e".into() },
                },
                Print {
                    expr: Var { name: "f".into() },
                },
                Print {
                    expr: Var { name: "g".into() },
                },
            ],
        },
        // call it once with seven distinct string literals
        Print {
            expr: FnCall {
                name: "print7".into(),
                args: vec![
                    Const {
                        value: Value::Str("one".into()),
                    },
                    Const {
                        value: Value::Str("two".into()),
                    },
                    Const {
                        value: Value::Str("three".into()),
                    },
                    Const {
                        value: Value::Str("four".into()),
                    },
                    Const {
                        value: Value::Str("five".into()),
                    },
                    Const {
                        value: Value::Str("six".into()),
                    },
                    Const {
                        value: Value::Str("seven".into()),
                    },
                ],
            },
        },
    ];

    let mut codegen = Codegen::default();
    println!("{}", codegen.run(&print_many));
}
