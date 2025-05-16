use lc1::{
    ast::{Expr, Stmt},
    codegen::Codegen,
    interpreter::Interpreter,
};

use Expr::*;
use Stmt::*;

fn main() {
    let program = vec![
        FnDecl {
            name: "add".to_string(),
            args: vec![
                "a".to_string(),
                "b".to_string(),
                "c".to_string(),
                "d".to_string(),
                "e".to_string(),
                "f".to_string(),
                "g".to_string(),
            ],
            body: vec![Return {
                expr: Add {
                    lhs: Box::new(Var {
                        name: "a".to_string(),
                    }),
                    rhs: Box::new(Add {
                        lhs: Box::new(Var {
                            name: "b".to_string(),
                        }),
                        rhs: Box::new(Add {
                            lhs: Box::new(Var {
                                name: "c".to_string(),
                            }),
                            rhs: Box::new(Add {
                                lhs: Box::new(Var {
                                    name: "d".to_string(),
                                }),
                                rhs: Box::new(Add {
                                    lhs: Box::new(Var {
                                        name: "e".to_string(),
                                    }),
                                    rhs: Box::new(Add {
                                        lhs: Box::new(Var {
                                            name: "f".to_string(),
                                        }),
                                        rhs: Box::new(Var {
                                            name: "g".to_string(),
                                        }),
                                    }),
                                }),
                            }),
                        }),
                    }),
                },
            }],
        },
        VarDecl {
            name: "a".to_string(),
            value: Const { value: 1 },
        },
        VarDecl {
            name: "b".to_string(),
            value: Const { value: 1 },
        },
        Print {
            expr: FnCall {
                name: "add".to_string(),
                args: vec![
                    Var {
                        name: "a".to_string(),
                    },
                    Var {
                        name: "b".to_string(),
                    },
                    Var {
                        name: "a".to_string(),
                    },
                    Var {
                        name: "b".to_string(),
                    },
                    Var {
                        name: "a".to_string(),
                    },
                    Var {
                        name: "b".to_string(),
                    },
                    Var {
                        name: "a".to_string(),
                    },
                ],
            },
        },
        Print {
            expr: Var {
                name: "b".to_string(),
            },
        },
        If {
            cond: Const { value: 1 },
            then: vec![Print {
                expr: Const { value: 10 },
            }],
            else_branch: Some(vec![Print {
                expr: Const { value: 20 },
            }]),
        },
        Return {
            expr: Add {
                lhs: Box::new(Var {
                    name: "a".to_string(),
                }),
                rhs: Box::new(Var {
                    name: "b".to_string(),
                }),
            },
        },
    ];
    let if_program = vec![If {
        cond: Const { value: 0 },
        then: vec![Print {
            expr: Const { value: 10 },
        }],
        else_branch: Some(vec![Print {
            expr: Const { value: 20 },
        }]),
    }];
    let nested_if = vec![
        VarDecl {
            name: "x".to_string(),
            value: Const { value: 1 },
        },
        If {
            cond: Const { value: 1 },
            then: vec![
                VarDecl {
                    name: "x".to_string(),
                    value: Const { value: 2 },
                },
                Print {
                    expr: Var {
                        name: "x".to_string(),
                    },
                },
            ],
            else_branch: None,
        },
        Print {
            expr: Var {
                name: "x".to_string(),
            },
        },
    ];
    let fibonacci_program = vec![
        // Declare fibonacci function
        FnDecl {
            name: "fibonacci".to_string(),
            args: vec!["n".to_string()],
            body: vec![If {
                cond: Lt {
                    lhs: Box::new(Var {
                        name: "n".to_string(),
                    }),
                    rhs: Box::new(Const { value: 2 }),
                },
                then: vec![Return {
                    expr: Var {
                        name: "n".to_string(),
                    },
                }],
                else_branch: Some(vec![Return {
                    expr: Add {
                        lhs: Box::new(FnCall {
                            name: "fibonacci".to_string(),
                            args: vec![Add {
                                lhs: Box::new(Var {
                                    name: "n".to_string(),
                                }),
                                rhs: Box::new(Const { value: -1 }),
                            }],
                        }),
                        rhs: Box::new(FnCall {
                            name: "fibonacci".to_string(),
                            args: vec![Add {
                                lhs: Box::new(Var {
                                    name: "n".to_string(),
                                }),
                                rhs: Box::new(Const { value: -2 }),
                            }],
                        }),
                    },
                }]),
            }],
        },
        Print {
            expr: FnCall {
                name: "fibonacci".to_string(),
                args: vec![Const { value: 20 }],
            },
        },
    ];
    let mut codegen = Codegen::default();
    println!("{}", codegen.run(&fibonacci_program));

    // let mut interpreter = Interpreter::default();
    // interpreter.run(&fibonacci_program);
}
