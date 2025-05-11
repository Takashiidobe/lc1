use std::{cmp::max, collections::HashMap};

use crate::ast::{Expr, Stmt};

#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct Codegen {
    output: String,
    label_count: usize,
    var_offsets: HashMap<String, i64>, // stack offsets
    stack_offset: i64,
    max_offset: u64,
    in_function: bool,
}

fn arg_reg(index: usize) -> &'static str {
    match index {
        0 => "%rdi",
        1 => "%rsi",
        2 => "%rdx",
        3 => "%rcx",
        4 => "%r8",
        5 => "%r9",
        _ => panic!("More than 6 arguments not yet supported"),
    }
}

impl Codegen {
    pub fn new() -> Self {
        Self::default()
    }

    fn emit(&mut self, line: &str) {
        self.output.push_str("    ");
        self.output.push_str(line);
        self.output.push('\n');
    }

    fn new_label(&mut self, prefix: &str) -> String {
        let label = format!("{}_{}", prefix, self.label_count);
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
        // calculate max offset first
        self.stack_offset(stmts);

        self.emit(".section .data");
        self.emit("fmt: .string \"%ld\\n\"");
        self.emit(".text");
        self.emit(".globl main");
        self.emit("");

        for stmt in stmts {
            if let Stmt::FnDecl { .. } = stmt {
                self.gen_stmt(stmt);
            }
        }

        self.emit("main:");
        self.emit("    pushq %rbp");
        self.emit("    movq %rsp, %rbp");
        self.emit(&format!("    subq ${}, %rsp", self.max_offset * 8));

        for stmt in stmts {
            if !matches!(stmt, Stmt::FnDecl { .. }) {
                self.gen_stmt(stmt);
            }
        }

        self.output.clone()
    }

    fn gen_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VarDecl { name, value } => {
                self.gen_expr(value);
                self.stack_offset -= 8;
                self.emit(&format!("movq %rax, {}(%rbp)", self.stack_offset));
                self.var_offsets.insert(name.clone(), self.stack_offset);
            }
            Stmt::Print { expr } => {
                self.gen_expr(expr);
                self.emit("movq %rax, %rsi");
                self.emit("leaq fmt(%rip), %rdi");
                self.emit("xor %rax, %rax");
                self.emit("call printf");
            }
            Stmt::Return { expr } => {
                self.gen_expr(expr);
                if !self.in_function {
                    self.emit(&format!("    addq ${}, %rsp", self.max_offset * 8));
                }
                self.emit("    popq %rbp");
                self.emit("    ret");
            }
            Stmt::FnDecl { name, args, body } => {
                self.in_function = true;
                self.emit(&format!("{}:", name));
                self.emit("    pushq %rbp");
                self.emit("    movq %rsp, %rbp");
                for (i, arg) in args.iter().enumerate() {
                    let offset = -8 * (i as i64 + 1);
                    self.emit(&format!("    movq {}, {}(%rbp)", arg_reg(i), offset));
                    self.var_offsets.insert(arg.clone(), offset);
                }
                self.stack_offset = -8 * args.len() as i64;

                for stmt in body {
                    self.gen_stmt(stmt);
                }

                self.in_function = false;
                self.stack_offset = 0;
                self.var_offsets.clear();
            }
        }
    }

    fn gen_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Const { value } => {
                self.emit(&format!("    movq ${}, %rax", value));
            }
            Expr::Var { name } => {
                if let Some(offset) = self.var_offsets.get(name) {
                    self.emit(&format!("    movq {}(%rbp), %rax", offset));
                } else {
                    panic!("Undefined variable: {}", name);
                }
            }
            Expr::Add { lhs, rhs } => {
                self.gen_expr(rhs);
                self.emit("    pushq %rax");
                self.gen_expr(lhs);
                self.emit("    popq %rbx");
                self.emit("    addq %rbx, %rax");
            }
            Expr::FnCall { name, args } => {
                for (i, arg) in args.iter().enumerate() {
                    self.gen_expr(arg);
                    self.emit(&format!("    movq %rax, {}", arg_reg(i)));
                }
                self.emit(&format!("    call {}", name));
            }
        }
    }
}
