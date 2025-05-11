use std::{collections::HashMap, process::exit};

use crate::ast::{Expr, Stmt};

// I need a set of nested hashmaps for stack frames
#[derive(Default, Clone, PartialEq, Eq)]
pub struct Interpreter {
    fns: HashMap<String, (Vec<String>, Vec<Stmt>)>,
    vars: HashMap<String, Expr>,
}

impl Interpreter {
    pub fn run(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.exec(stmt);
        }
    }

    pub fn exec(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::FnDecl { name, args, body } => {
                self.fns
                    .insert(name.clone(), (args.to_vec(), body.to_vec()));
            }
            Stmt::VarDecl { name, value } => {
                self.vars.insert(name.clone(), value.clone());
            }
            Stmt::Print { expr } => {
                let expr = self.eval(expr);
                println!("{:?}", expr)
            }
            Stmt::Return { expr } => {
                let val = self.eval(expr);
                match val {
                    Expr::Const { value } => {
                        exit(value.try_into().unwrap());
                    }
                    _ => panic!("Could not return with value"),
                }
            }
        }
    }

    pub fn eval(&mut self, expr: &Expr) -> Expr {
        match expr {
            Expr::Add { lhs, rhs } => {
                let lhs = self.eval(lhs);
                let rhs = self.eval(rhs);
                match (lhs, rhs) {
                    (Expr::Const { value: l }, Expr::Const { value: r }) => {
                        Expr::Const { value: l + r }
                    }
                    _ => panic!("Cannot add two non-number expressions"),
                }
            }
            Expr::Const { .. } => expr.clone(),
            Expr::Var { name } => self.vars.get(name).unwrap().clone(),
            Expr::FnCall { name, args } => {
                let (vars, body) = self.fns.get(name).unwrap().clone();
                if args.len() != vars.len() {
                    panic!("cannot call this function without correct arity");
                }
                for (var, arg) in vars.iter().zip(args) {
                    let evaled_arg = self.eval(arg);
                    self.vars.insert(var.to_string(), evaled_arg);
                }

                let mut ret_val = None;
                for stmt in body {
                    match stmt {
                        Stmt::Return { expr } => {
                            ret_val = Some(self.eval(&expr));
                            break;
                        }
                        _ => {
                            self.exec(&stmt);
                        }
                    }
                }

                for var in vars.clone() {
                    self.vars.remove(&var);
                }

                match ret_val {
                    Some(v) => v,
                    None => Expr::Const { value: 0 },
                }
            }
        }
    }
}
