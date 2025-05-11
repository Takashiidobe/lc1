use lc1::{
    ast::{Expr, Stmt},
    codegen::Codegen,
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
    let mut codegen = Codegen::default();

    println!("{}", codegen.run(&program));

    // let mut interpreter = Interpreter::default();
    // dbg!(interpreter.run(&program));
}
