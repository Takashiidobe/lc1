use std::{cmp::max, collections::HashMap};

use crate::ast::{Expr, Stmt, Value};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Int,
    Str,
    Null,
    Array(Box<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Codegen {
    output: Vec<String>,
    label_count: usize,
    var_offsets: HashMap<String, i64>,
    stack_offset: i64,
    max_offset: u64,
    string_literals: HashMap<String, String>,
    fn_ret_types: HashMap<String, Type>,
    var_types: Vec<HashMap<String, Type>>,
}

impl Default for Codegen {
    fn default() -> Self {
        Self {
            output: Default::default(),
            label_count: Default::default(),
            var_offsets: Default::default(),
            stack_offset: Default::default(),
            max_offset: Default::default(),
            string_literals: Default::default(),
            fn_ret_types: Default::default(),
            var_types: vec![Default::default()],
        }
    }
}

fn arg_reg(index: usize) -> Option<&'static str> {
    match index {
        0 => Some("%rdi"),
        1 => Some("%rsi"),
        2 => Some("%rdx"),
        3 => Some("%rcx"),
        4 => Some("%r8"),
        5 => Some("%r9"),
        _ => None,
    }
}

impl Codegen {
    pub fn new() -> Self {
        Self::default()
    }

    fn emit(&mut self, line: &str) {
        self.output.push(line.trim().to_string());
    }

    fn enter_scope(&mut self) {
        self.var_types.push(HashMap::new());
    }

    fn exit_scope(&mut self) {
        self.var_types.pop();
    }

    fn insert_var_type(&mut self, name: String, ty: Type) {
        self.var_types
            .last_mut()
            .expect("no scope open")
            .insert(name, ty);
    }

    fn lookup_var_type(&self, name: &str) -> Type {
        for scope in self.var_types.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return ty.clone();
            }
        }
        panic!("unknown variable `{}`", name);
    }

    fn type_of_expr(&self, expr: &Expr) -> Type {
        use Expr::*;
        match expr {
            Const { value } => match value {
                Value::Int(_) => Type::Int,
                Value::Str(_) => Type::Str,
                Value::Null => Type::Null,
                Value::Array(items) => {
                    let ty = items
                        .first()
                        .map(|value| {
                            self.type_of_expr(&Expr::Const {
                                value: value.clone(),
                            })
                        })
                        .unwrap_or(Type::Null);
                    Type::Array(Box::new(ty))
                }
            },
            Var { name } => self.lookup_var_type(name),
            FnCall { name, .. } => self
                .fn_ret_types
                .get(name.as_str())
                .unwrap_or_else(|| panic!("unknown fn `{}`", name))
                .clone(),
            Expr::Array { items } => {
                let ty = items
                    .first()
                    .map(|i| self.type_of_expr(i))
                    .unwrap_or(Type::Null);
                Type::Array(Box::new(ty))
            }
            _ => Type::Int,
        }
    }

    fn new_label(&mut self, prefix: &str) -> String {
        let label = format!(".L{}_{}", prefix, self.label_count);
        self.label_count += 1;
        label
    }

    fn stack_offset(&mut self, stmts: &[Stmt]) {
        fn count_vars(stmts: &[Stmt]) -> u64 {
            stmts
                .iter()
                .filter(|x| matches!(x, Stmt::VarDecl { .. }))
                .count() as u64
        }

        let mut var_count = 0;
        let mut global_count = 0;

        for stmt in stmts {
            match stmt {
                Stmt::FnDecl { body, args, .. } => {
                    let function_vars = count_vars(body) + args.len() as u64;
                    var_count = max(var_count, function_vars);
                }
                Stmt::VarDecl { .. } => global_count += 1,
                _ => {}
            }
        }

        self.max_offset = max(var_count, global_count);
    }

    pub fn run(&mut self, stmts: &[Stmt]) -> String {
        self.stack_offset(stmts);
        self.collect_string_literals(stmts);

        self.emit(".section .data");
        self.emit(r#"Larr_open:  .string "[""#);
        self.emit(r#"Larr_sep:   .string ", ""#);
        self.emit(r#"Larr_close: .string "]""#);
        self.emit("fmt: .string \"%ld\"");
        self.emit("printf_str: .string \"%s\"");
        self.emit("null: .string \"null\"");

        self.emit(".section .rodata");
        let literals = self.string_literals.clone();
        for (s, label) in &literals {
            let esc = s.escape_default().to_string();
            self.emit(&format!("{}: .string \"{}\"", label, esc));
        }

        self.emit(".text");
        self.emit(".globl main");
        self.emit("");

        for stmt in stmts {
            if let Stmt::FnDecl { .. } = stmt {
                self.gen_stmt(stmt);
            }
        }

        self.emit("main:");
        self.emit("pushq %rbp");
        self.emit("movq %rsp, %rbp");
        self.emit(&format!("subq ${}, %rsp", self.max_offset * 8));

        for stmt in stmts {
            if !matches!(stmt, Stmt::FnDecl { .. }) {
                self.gen_stmt(stmt);
            }
        }

        self.emit("xor %rax, %rax");
        self.emit(&format!("addq ${}, %rsp", self.max_offset * 8));
        self.emit("popq %rbp");
        self.emit("ret");

        self.format_asm()
    }

    fn emit_print_reg(&mut self, ty: &Type) {
        match ty {
            Type::Int => {
                self.emit("movq %rax, %rsi");
                self.emit("leaq fmt(%rip), %rdi");
                self.emit("xor  %rax, %rax");
                self.emit("call printf");
            }
            Type::Str | Type::Null => {
                self.emit("movq %rax, %rsi");
                self.emit("leaq printf_str(%rip), %rdi");
                self.emit("xor  %rax, %rax");
                self.emit("call printf");
            }
            Type::Array(_) => {
                self.emit("movq %rax, %r12");
                self.emit("movq (%r12), %r13");
                self.emit("xorq %r14, %r14");
            }
        }
    }

    fn emit_print_array(&mut self, elem_ty: &Type) {
        self.emit("movq %rax, %r12");
        self.emit("movq (%r12), %r13");
        self.emit("xorq %r14, %r14");

        self.emit("xor  %rax, %rax");
        self.emit("leaq Larr_open(%rip), %rsi");
        self.emit("leaq printf_str(%rip), %rdi");
        self.emit("call printf");

        let loop_lbl = self.new_label("array_loop");
        let no_sep_lbl = self.new_label("array_no_sep");
        let end_lbl = self.new_label("array_end");

        self.emit(&format!("{}:", loop_lbl));
        self.emit("cmpq %r13, %r14");
        self.emit(&format!("jge {}", end_lbl));

        self.emit("cmpq $0, %r14");
        self.emit(&format!("je {}", no_sep_lbl));
        self.emit("xor  %rax, %rax");
        self.emit("leaq Larr_sep(%rip), %rsi");
        self.emit("leaq printf_str(%rip), %rdi");
        self.emit("call printf");

        self.emit(&format!("{}:", no_sep_lbl));

        self.emit("movq 8(%r12,%r14,8), %rax");

        match elem_ty {
            Type::Int => {
                self.emit("movq %rax, %rsi");
                self.emit("leaq fmt(%rip), %rdi");
                self.emit("xor  %rax, %rax");
                self.emit("call printf");
            }

            Type::Str | Type::Null => {
                self.emit("movq %rax, %rsi");
                self.emit("leaq printf_str(%rip), %rdi");
                self.emit("xor  %rax, %rax");
                self.emit("call printf");
            }
            Type::Array(inner_ty) => {
                self.emit("pushq %r12");
                self.emit("pushq %r13");
                self.emit("pushq %r14");

                self.emit_print_array(inner_ty);

                self.emit("popq %r14");
                self.emit("popq %r13");
                self.emit("popq %r12");
            }
        }

        self.emit("incq %r14");
        self.emit(&format!("jmp {}", loop_lbl));

        self.emit(&format!("{}:", end_lbl));
        self.emit("xor  %rax, %rax");
        self.emit("leaq Larr_close(%rip), %rsi");
        self.emit("leaq printf_str(%rip), %rdi");
        self.emit("call printf");
    }

    fn gen_print_value(&mut self, expr: &Expr, ty: &Type) {
        self.gen_expr(expr);

        match ty {
            Type::Array(elem_ty) => {
                self.emit_print_array(elem_ty);

                if let Type::Array(_) = ty {
                    self.emit("movq %r12, %rdi");
                    self.emit("call free");
                }
            }
            _ => self.emit_print_reg(ty),
        }
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VarDecl { name, value } => {
                let ty = self.type_of_expr(value);
                self.insert_var_type(name.clone(), ty);
                self.gen_expr(value);
                self.stack_offset -= 8;
                self.emit(&format!("movq %rax, {}(%rbp)", self.stack_offset));
                self.var_offsets.insert(name.clone(), self.stack_offset);
            }
            Stmt::Print { expr } => {
                let ty = self.type_of_expr(expr);

                self.gen_print_value(expr, &ty);
            }
            Stmt::Return { expr } => {
                self.gen_expr(expr);
                if self.stack_offset < -8 * self.var_offsets.len() as i64 {
                    let cleanup = -self.stack_offset - (8 * self.var_offsets.len() as i64);
                    self.emit(&format!("addq ${}, %rsp", cleanup));
                }
                self.emit("movq %rbp, %rsp");
                self.emit("popq %rbp");
                self.emit("ret");
            }
            Stmt::FnDecl {
                name,
                args,
                body,
                return_type,
            } => {
                self.emit(&format!("{}:", name));
                self.emit("pushq %rbp");
                self.emit("movq %rsp, %rbp");
                let frame_bytes = (self.max_offset as i64) * 8;
                self.emit(&format!("subq ${}, %rsp", frame_bytes));

                self.fn_ret_types.insert(name.clone(), return_type.clone());

                self.enter_scope();
                for (arg_name, arg_type) in args {
                    self.insert_var_type(arg_name.clone(), arg_type.clone());
                }

                for (i, (arg, _)) in args.iter().enumerate() {
                    let offset = -8 * (i as i64 + 1);
                    if let Some(reg) = arg_reg(i) {
                        self.emit(&format!("movq {}, {}(%rbp)", reg, offset));
                    } else {
                        let stack_offset = 16 + 8 * ((i as i64) - 6);
                        self.emit(&format!("movq {}(%rbp), %rax", stack_offset));
                        self.emit(&format!("movq %rax, {}(%rbp)", offset));
                    }
                    self.var_offsets.insert(arg.clone(), offset);
                }

                self.stack_offset = -8 * args.len() as i64;

                let function_start_offset = self.stack_offset;

                let mut has_explicit_return = false;
                for (i, stmt) in body.iter().enumerate() {
                    self.gen_stmt(stmt);
                    if i == body.len() - 1 {
                        if let Stmt::Return { .. } = stmt {
                            has_explicit_return = true;
                        }
                    }
                }

                if !has_explicit_return {
                    self.gen_expr(&Expr::Const { value: Value::Null });
                }

                if self.stack_offset < function_start_offset {
                    let stack_cleanup = function_start_offset - self.stack_offset;
                    self.emit(&format!("addq ${}, %rsp", stack_cleanup));
                }

                if !has_explicit_return {
                    self.emit("movq %rbp, %rsp");
                    self.emit("popq %rbp");
                    self.emit("ret");
                }

                self.stack_offset = 0;
                self.var_offsets.clear();
                self.exit_scope();
            }
            Stmt::If {
                cond,
                then,
                else_branch,
            } => {
                let else_label = self.new_label("else");
                let end_label = self.new_label("end");

                self.gen_expr(cond);

                self.emit("test %rax, %rax");

                if else_branch.is_some() {
                    self.emit(&format!("jz {}", else_label));
                } else {
                    self.emit(&format!("jz {}", end_label));
                }
                let then_returns = then
                    .last()
                    .is_some_and(|stmt| matches!(stmt, Stmt::Return { .. }));

                self.enter_scope();
                for stmt in then {
                    self.gen_stmt(stmt);
                }
                self.exit_scope();

                if else_branch.is_some() && !then_returns {
                    self.emit(&format!("jmp {}", end_label));
                }

                if let Some(else_stmts) = else_branch {
                    self.emit(&format!("{}:", else_label));

                    self.enter_scope();
                    for stmt in else_stmts {
                        self.gen_stmt(stmt);
                    }
                    self.exit_scope();
                }

                let needs_end_label = if let Some(else_stmts) = else_branch {
                    !then_returns
                        || !else_stmts
                            .last()
                            .is_some_and(|stmt| matches!(stmt, Stmt::Return { .. }))
                } else {
                    true
                };

                if needs_end_label {
                    self.emit(&format!("{}:", end_label));
                }
            }
        }
    }

    fn gen_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Const { value } => match value {
                Value::Int(i) => {
                    self.emit(&format!("movq ${}, %rax", i));
                }
                Value::Str(s) => {
                    let label = &self.string_literals.get(s).unwrap();
                    self.emit(&format!("leaq {}(%rip), %rax", label));
                }
                Value::Null => {
                    self.emit("leaq null(%rip), %rax");
                }
                Value::Array(_) => {}
            },
            Expr::Var { name } => {
                if let Some(offset) = self.var_offsets.get(name) {
                    self.emit(&format!("movq {}(%rbp), %rax", offset));
                } else {
                    panic!("Undefined variable: {}", name);
                }
            }
            Expr::Add { lhs, rhs } => {
                self.gen_expr(lhs);
                self.emit("pushq %rax");
                self.gen_expr(rhs);
                self.emit("popq %rbx");
                self.emit("addq %rbx, %rax");
            }
            Expr::Lt { lhs, rhs } => {
                self.gen_expr(lhs);
                self.emit("pushq %rax");
                self.gen_expr(rhs);
                self.emit("popq %rbx");
                self.emit("cmpq %rax, %rbx");
                self.emit("setl %al");
                self.emit("movzbq %al, %rax");
            }
            Expr::Le { lhs, rhs } => {
                self.gen_expr(lhs);
                self.emit("pushq %rax");
                self.gen_expr(rhs);
                self.emit("popq %rbx");
                self.emit("cmpq %rax, %rbx");
                self.emit("setle %al");
                self.emit("movzbq %al, %rax");
            }
            Expr::Gt { lhs, rhs } => {
                self.gen_expr(lhs);
                self.emit("pushq %rax");
                self.gen_expr(rhs);
                self.emit("popq %rbx");
                self.emit("cmpq %rax, %rbx");
                self.emit("setg %al");
                self.emit("movzbq %al, %rax");
            }
            Expr::Ge { lhs, rhs } => {
                self.gen_expr(lhs);
                self.emit("pushq %rax");
                self.gen_expr(rhs);
                self.emit("popq %rbx");
                self.emit("cmpq %rax, %rbx");
                self.emit("setge %al");
                self.emit("movzbq %al, %rax");
            }
            Expr::Eq { lhs, rhs } => {
                self.gen_expr(lhs);
                self.emit("pushq %rax");
                self.gen_expr(rhs);
                self.emit("popq %rbx");
                self.emit("cmpq %rax, %rbx");
                self.emit("sete %al");
                self.emit("movzbq %al, %rax");
            }
            Expr::Ne { lhs, rhs } => {
                self.gen_expr(lhs);
                self.emit("pushq %rax");
                self.gen_expr(rhs);
                self.emit("popq %rbx");
                self.emit("cmpq %rax, %rbx");
                self.emit("setne %al");
                self.emit("movzbq %al, %rax");
            }
            Expr::FnCall { name, args } => {
                let mut stack_arg_count = 0;
                for (i, arg) in args.iter().enumerate() {
                    self.gen_expr(arg);
                    if let Some(reg) = arg_reg(i) {
                        self.emit(&format!("movq %rax, {}", reg));
                    } else {
                        self.emit("pushq %rax");
                        stack_arg_count += 1;
                    }
                }

                self.emit(&format!("call {}", name));

                if stack_arg_count > 0 {
                    self.emit(&format!("addq ${}, %rsp", 8 * stack_arg_count));
                }
            }
            Expr::Neg { expr } => {
                self.gen_expr(expr);
                self.emit("negq %rax");
            }
            Expr::Array { items } => {
                let n = items.len() as i64;
                let total_bytes = 8 * (1 + n);

                self.emit(&format!("movq ${}, %rdi", total_bytes));
                self.emit("call malloc");

                self.emit("movq %rax, %r12");
                self.emit(&format!("movq ${}, (%r12)", n));

                for (i, item) in items.iter().enumerate() {
                    let off = 8 * (i as i64 + 1);
                    self.emit("pushq %r12");
                    self.gen_expr(item);
                    self.emit("popq %r12");
                    self.emit(&format!("movq %rax, {}(%r12)", off));
                }

                self.emit("movq %r12, %rax");
            }
        }
    }

    fn collect_string_literals(&mut self, stmts: &[Stmt]) {
        fn walk_expr(cg: &mut Codegen, expr: &Expr) {
            match expr {
                Expr::Const {
                    value: Value::Str(s),
                } => {
                    if !cg.string_literals.contains_key(s) {
                        let lbl = cg.new_label("str");
                        cg.string_literals.insert(s.clone(), lbl);
                    }
                }
                Expr::Add { lhs, rhs } | Expr::Lt { lhs, rhs } => {
                    walk_expr(cg, lhs);
                    walk_expr(cg, rhs);
                }
                Expr::FnCall { args, .. } => {
                    for arg in args {
                        walk_expr(cg, arg);
                    }
                }
                _ => {}
            }
        }

        for stmt in stmts {
            match stmt {
                Stmt::FnDecl { body, .. } => {
                    self.collect_string_literals(body);
                }
                Stmt::VarDecl { value, .. } => {
                    walk_expr(self, value);
                }
                Stmt::Print { expr } | Stmt::Return { expr } => {
                    walk_expr(self, expr);
                }
                Stmt::If {
                    cond,
                    then,
                    else_branch,
                } => {
                    walk_expr(self, cond);
                    self.collect_string_literals(then);
                    if let Some(else_stmts) = else_branch {
                        self.collect_string_literals(else_stmts);
                    }
                }
            }
        }
    }

    fn format_asm(&self) -> String {
        self.output
            .iter()
            .map(|line| {
                if line.ends_with(':') || line.starts_with('.') || line.is_empty() {
                    line.to_string()
                } else {
                    format!("  {}", line)
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }
}
