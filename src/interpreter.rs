use crate::{
    ast::{Expr, Stmt, Value},
    codegen::Type,
};
use std::collections::HashMap;

#[derive(Default, Clone, PartialEq, Eq)]
pub struct StackFrame {
    vars: HashMap<String, Value>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Interpreter {
    fns: HashMap<String, (Vec<(String, Type)>, Vec<Stmt>)>,
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
                    Value::Int(i) => std::process::exit(i as i32),
                    Value::Str(s) => {
                        println!("{}", s);
                        std::process::exit(0);
                    }
                    Value::Null => std::process::exit(0),
                    Value::Array(_) => std::process::exit(0),
                }
            }
        }
    }

    /// Execute one statement; returns Some(Value) if it was a `return`.
    pub fn exec(&mut self, stmt: &Stmt) -> Option<Value> {
        match stmt {
            Stmt::FnDecl {
                name, args, body, ..
            } => {
                let names = args.clone();
                self.fns.insert(name.clone(), (names, body.clone()));
                None
            }
            Stmt::VarDecl { name, value } => {
                let v = self.eval(value);
                self.current_frame_mut().vars.insert(name.clone(), v);
                None
            }
            Stmt::Print { expr } => {
                let v = self.eval(expr);
                println!("{}", v);
                None
            }
            Stmt::Return { expr } => Some(self.eval(expr)),
            Stmt::If {
                cond,
                then,
                else_branch,
            } => {
                let cv = self.eval(cond);
                let truthy = match cv {
                    Value::Int(i) => i != 0,
                    _ => panic!("Condition must evaluate to an integer"),
                };

                if truthy {
                    if let Some(ret) = self.exec_block(then, StackFrame::default()) {
                        return Some(ret);
                    }
                } else if let Some(els) = else_branch {
                    if let Some(ret) = self.exec_block(els, StackFrame::default()) {
                        return Some(ret);
                    }
                }

                None
            }
        }
    }

    /// Evaluate an expression to a Value.
    pub fn eval(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Neg { expr } => {
                let v = self.eval(expr);
                match v {
                    Value::Int(i) => Value::Int(-i),
                    _ => panic!("Unary minus on non-int"),
                }
            }
            Expr::Add { lhs, rhs } => {
                let l = self.eval(lhs);
                let r = self.eval(rhs);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                    _ => panic!("Addition requires two ints"),
                }
            }
            Expr::Lt { lhs, rhs } => {
                let l = self.eval(lhs);
                let r = self.eval(rhs);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(if a < b { 1 } else { 0 }),
                    _ => panic!("Comparison requires two ints"),
                }
            }
            Expr::Le { lhs, rhs } => {
                let l = self.eval(lhs);
                let r = self.eval(rhs);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(if a <= b { 1 } else { 0 }),
                    _ => panic!("Comparison requires two ints"),
                }
            }
            Expr::Gt { lhs, rhs } => {
                let l = self.eval(lhs);
                let r = self.eval(rhs);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(if a > b { 1 } else { 0 }),
                    _ => panic!("Comparison requires two ints"),
                }
            }
            Expr::Ge { lhs, rhs } => {
                let l = self.eval(lhs);
                let r = self.eval(rhs);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Value::Int(if a >= b { 1 } else { 0 }),
                    _ => panic!("Comparison requires two ints"),
                }
            }
            Expr::Eq { lhs, rhs } => {
                let l = self.eval(lhs);
                let r = self.eval(rhs);
                let result = match (l, r) {
                    (Value::Int(a), Value::Int(b)) => a == b,
                    (Value::Str(ref a), Value::Str(ref b)) => a == b,
                    (Value::Null, Value::Null) => true,
                    _ => false,
                };
                Value::Int(if result { 1 } else { 0 })
            }
            Expr::Ne { lhs, rhs } => {
                let l = self.eval(lhs);
                let r = self.eval(rhs);
                let result = match (l, r) {
                    (Value::Int(a), Value::Int(b)) => a != b,
                    (Value::Str(ref a), Value::Str(ref b)) => a != b,
                    (Value::Null, Value::Null) => false,
                    _ => true,
                };
                Value::Int(if result { 1 } else { 0 })
            }
            Expr::Const { value } => value.clone(),
            Expr::Var { name } => {
                for frame in self.stack.iter().rev() {
                    if let Some(v) = frame.vars.get(name) {
                        return v.clone();
                    }
                }
                panic!("Undefined variable: {}", name);
            }
            Expr::FnCall { name, args } => {
                let (params, body) = self
                    .fns
                    .get(name)
                    .unwrap_or_else(|| panic!("Unknown fn: {}", name))
                    .clone();
                if params.len() != args.len() {
                    panic!(
                        "Wrong # args for {}: got {}, expected {}",
                        name,
                        args.len(),
                        params.len()
                    );
                }

                let mut frame = StackFrame::default();
                for (p, arg_expr) in params.iter().zip(args.iter()) {
                    let v = self.eval(arg_expr);
                    frame.vars.insert(p.0.clone(), v);
                }

                if let Some(ret) = self.exec_block(&body, frame) {
                    ret
                } else {
                    Value::Int(0)
                }
            }
            Expr::Array { items } => {
                let mut values = Vec::with_capacity(items.len());
                for item in items {
                    values.push(self.eval(item));
                }
                Value::Array(values)
            }
        }
    }

    fn current_frame_mut(&mut self) -> &mut StackFrame {
        self.stack.last_mut().unwrap()
    }

    fn exec_block(&mut self, stmts: &[Stmt], frame: StackFrame) -> Option<Value> {
        self.stack.push(frame);
        let mut result = None;
        for stmt in stmts {
            if let Some(v) = self.exec(stmt) {
                result = Some(v);
                break;
            }
        }
        self.stack.pop();
        result
    }
}
