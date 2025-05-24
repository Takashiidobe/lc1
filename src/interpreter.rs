use crate::{
    ast::{Expr, Stmt, Value},
    codegen::Type,
};
use std::{
    collections::{BTreeMap, HashMap},
    io::{self, Write as _},
};

#[derive(Default, Clone, PartialEq, Eq)]
pub struct StackFrame {
    vars: HashMap<String, Value>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Interpreter {
    fns: HashMap<String, (Vec<(String, Type)>, Vec<Stmt>)>,
    stack: Vec<StackFrame>,
    structs: HashMap<String, Vec<String>>,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self {
            fns: Default::default(),
            stack: vec![StackFrame::default()],
            structs: Default::default(),
        }
    }
}

impl Interpreter {
    pub fn run(&mut self, stmts: &[Stmt]) {
        for stmt in stmts {
            self.exec(stmt);
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
            Stmt::StructDecl { name, fields } => {
                if let Some(_) = self.structs.insert(
                    name.clone(),
                    fields.iter().map(|(f, _)| f.clone()).collect(),
                ) {
                    panic!("Duplicate struct `{name}`");
                }
                None
            }
            Stmt::Print { expr } => {
                let v = self.eval(expr);
                // handle control chars
                match v {
                    Value::Str(s) => {
                        for c in s.chars() {
                            if c == '\n' {
                                println!();
                            }
                            if c == '\t' {
                                print!("\t");
                            } else {
                                print!("{c}");
                            }
                        }
                    }
                    _ => {
                        print!("{}", v);
                        io::stdout().flush().unwrap();
                    }
                }
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
            Stmt::Expr { expr } => {
                self.eval(expr);
                None
            }
            Stmt::For {
                init,
                cond,
                post,
                body,
            } => {
                if let Some(init_stmt) = init {
                    self.exec(init_stmt);
                }

                while self.eval(cond).is_truthy() {
                    for stmt in body {
                        self.exec(stmt);
                    }
                    if let Some(post_expr) = post {
                        self.eval(post_expr);
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
            Expr::Index { array, index } => {
                let arr_val = self.eval(array);
                let vec = if let Value::Array(v) = arr_val {
                    v
                } else {
                    panic!("Type error: attempted indexing on non-array {:?}", arr_val);
                };
                let idx = self.eval(index).to_int();
                if idx < 0 || (idx as usize) >= vec.len() {
                    panic!(
                        "Index out of bounds: {} on array of length {}",
                        idx,
                        vec.len()
                    );
                }
                vec[idx as usize].clone()
            }
            Expr::Assign { target, value } => {
                let rhs = self.eval(value);

                match &**target {
                    Expr::Index { .. } => {
                        let (root, idx_exprs) = collect_indices(target);
                        let var_name = if let Expr::Var { name } = root {
                            name.clone()
                        } else {
                            panic!("Can only assign into a variable’s array slot");
                        };

                        let idxs: Vec<usize> = idx_exprs
                            .iter()
                            .map(|e| {
                                let v = self.eval(e).to_int();
                                if v < 0 {
                                    panic!("Negative index {} on `{}`", v, var_name);
                                }
                                v as usize
                            })
                            .collect();

                        let entry = self
                            .current_frame_mut()
                            .vars
                            .get_mut(&var_name)
                            .unwrap_or_else(|| panic!("Undefined variable `{}`", var_name));

                        assign_into(entry, &idxs, rhs.clone(), &var_name);

                        rhs
                    }
                    Expr::Field { object, name } => {
                        let var_name = if let Expr::Var { name } = &**object {
                            name
                        } else {
                            panic!(
                                "Can only assign to a named struct instance: tried `{object:?}`"
                            );
                        };

                        let entry = self
                            .current_frame_mut()
                            .vars
                            .get_mut(var_name)
                            .unwrap_or_else(|| panic!("Undefined variable `{}`", var_name));

                        let map = if let Value::Struct(_, map) = entry {
                            map
                        } else {
                            panic!("Type error: `{}` is not a struct", var_name);
                        };

                        if !map.contains_key(name) {
                            panic!("Unknown field `{}` on struct `{}`", name, var_name);
                        }
                        map.insert(name.clone(), rhs.clone());

                        rhs
                    }
                    Expr::Var { name } => {
                        self.current_frame_mut()
                            .vars
                            .insert(name.clone(), rhs.clone());
                        rhs
                    }
                    other => {
                        panic!("Invalid assignment target: {:?}", other);
                    }
                }
            }
            Expr::Struct { name, fields } => {
                let def_fields = self
                    .structs
                    .get(name)
                    .unwrap_or_else(|| panic!("Unknown struct `{}`", name));

                if def_fields.len() != fields.len() {
                    panic!(
                        "Struct `{}` expects {} fields, got {}",
                        name,
                        def_fields.len(),
                        fields.len()
                    );
                }

                for (expected, (got, _)) in def_fields.iter().zip(fields.iter()) {
                    if expected != got {
                        panic!(
                            "Field mismatch in `{}`: expected `{}`, found `{}`",
                            name, expected, got
                        );
                    }
                }

                let mut map = BTreeMap::new();
                for (field_name, field_expr) in fields {
                    let v = self.eval(field_expr);
                    map.insert(field_name.clone(), v);
                }

                Value::Struct(name.clone(), map)
            }
            Expr::Field { object, name } => {
                let base = self.eval(object);
                match base {
                    Value::Struct(struct_name, map) => map
                        .get(name)
                        .unwrap_or_else(|| {
                            panic!("Unknown field `{}` on struct `{}`", name, struct_name)
                        })
                        .clone(),
                    other => panic!(
                        "Type error: tried to access field `{}` on non-struct {:?}",
                        name, other
                    ),
                }
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

fn collect_indices(expr: &Expr) -> (&Expr, Vec<&Expr>) {
    match expr {
        Expr::Index { array, index } => {
            let (root, mut rest) = collect_indices(array);
            rest.push(&**index);
            (root, rest)
        }
        other => (other, Vec::new()),
    }
}
fn assign_into(val: &mut Value, idxs: &[usize], rhs: Value, root_name: &str) {
    let vec = match val {
        Value::Array(v) => v,
        _ => panic!("Type error: `{}` is not an array", root_name),
    };

    let i = idxs[0];
    if i >= vec.len() {
        panic!(
            "Index out of bounds: {} on array `{}` (len = {})",
            i,
            root_name,
            vec.len()
        );
    }

    if idxs.len() == 1 {
        vec[i] = rhs;
    } else {
        assign_into(&mut vec[i], &idxs[1..], rhs, root_name);
    }
}
