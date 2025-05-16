use std::{collections::HashMap, process::exit};

use crate::ast::{Expr, Stmt};

#[derive(Default, Clone, PartialEq, Eq)]
pub struct StackFrame {
    vars: HashMap<String, Expr>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Interpreter {
    fns: HashMap<String, (Vec<String>, Vec<Stmt>)>,
    stack: Vec<StackFrame>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            fns: Default::default(),
            stack: vec![StackFrame::default()],
        }
    }
}

impl Interpreter {
    pub fn run(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            if let Some(val) = self.exec(stmt) {
                match val {
                    Expr::Const { value } => exit(value as i32),
                    _ => exit(0),
                }
            }
        }
    }

    pub fn exec(&mut self, stmt: &Stmt) -> Option<Expr> {
        match stmt {
            Stmt::FnDecl { name, args, body } => {
                self.fns
                    .insert(name.clone(), (args.to_vec(), body.to_vec()));
                None
            }
            Stmt::VarDecl { name, value } => {
                let value = self.eval(value);
                self.current_frame_mut().vars.insert(name.clone(), value);
                None
            }
            Stmt::Print { expr } => {
                let expr = self.eval(expr);
                println!("{}", expr);
                None
            }
            Stmt::Return { expr } => {
                let val = self.eval(expr);
                match val {
                    Expr::Const { .. } => Some(val),
                    _ => None,
                }
            }
            Stmt::If {
                cond,
                then,
                else_branch,
            } => {
                let cond_val = self.eval(cond);
                let is_true = match cond_val {
                    Expr::Const { value } => value != 0,
                    _ => panic!("Condition must evaluate to a constant"),
                };

                if is_true {
                    if let Some(ret) = self.exec_block(then, StackFrame::default()) {
                        return Some(ret);
                    }
                } else if let Some(else_stmts) = else_branch {
                    if let Some(ret) = self.exec_block(else_stmts, StackFrame::default()) {
                        return Some(ret);
                    }
                }

                None
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
            Expr::Var { name } => {
                for frame in self.stack.iter().rev() {
                    if let Some(value) = frame.vars.get(name) {
                        return value.clone();
                    }
                }
                panic!("Undefined variable: {}", name);
            }
            Expr::FnCall { name, args } => {
                let (param_names, body) = self.fns.get(name).unwrap().clone();
                if args.len() != param_names.len() {
                    panic!(
                        "Cannot call function {} with {} arguments, expected {}",
                        name,
                        args.len(),
                        param_names.len()
                    );
                }

                let mut new_frame = StackFrame::default();

                for (param_name, arg) in param_names.iter().zip(args) {
                    let evaled_arg = self.eval(arg);
                    new_frame.vars.insert(param_name.to_string(), evaled_arg);
                }

                let ret_val = self.exec_block(&body, new_frame);

                match ret_val {
                    Some(v) => v,
                    None => Expr::Const { value: 0 },
                }
            }
            Expr::Lt { lhs, rhs } => {
                let lhs = self.eval(lhs);
                let rhs = self.eval(rhs);
                match (lhs, rhs) {
                    (Expr::Const { value: l }, Expr::Const { value: r }) => Expr::Const {
                        value: if l < r { 1 } else { 0 },
                    },
                    _ => panic!("Cannot compare non-number expressions"),
                }
            }
        }
    }

    fn current_frame_mut(&mut self) -> &mut StackFrame {
        self.stack.last_mut().unwrap()
    }
    fn exec_block(&mut self, stmts: &[Stmt], frame: StackFrame) -> Option<Expr> {
        self.stack.push(frame);

        let mut result = None;
        for stmt in stmts {
            if let Some(ret_val) = self.exec(stmt) {
                result = Some(ret_val);
                break;
            }
        }

        self.stack.pop();

        result
    }
}
