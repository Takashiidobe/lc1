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
        self.output.push_str(line);
        self.output.push('\n');
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
                if self.stack_offset < -8 * self.var_offsets.len() as i64 {
                    let cleanup = -self.stack_offset - (8 * self.var_offsets.len() as i64);
                    self.emit(&format!("addq ${}, %rsp", cleanup));
                }
                self.emit("movq %rbp, %rsp");
                self.emit("popq %rbp");
                self.emit("ret");
            }
            Stmt::FnDecl { name, args, body } => {
                self.in_function = true;
                self.emit(&format!("{}:", name));
                self.emit("pushq %rbp");
                self.emit("movq %rsp, %rbp");
                let frame_bytes = (self.max_offset as i64) * 8;
                self.emit(&format!("subq ${}, %rsp", frame_bytes));
                for (i, arg) in args.iter().enumerate() {
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
                    self.emit("movq $0, %rax");
                }

                if self.stack_offset < function_start_offset {
                    let stack_cleanup = function_start_offset - self.stack_offset;
                    self.emit(&format!("addq ${}, %rsp", stack_cleanup));
                }

                self.emit("movq %rbp, %rsp");
                self.emit("popq %rbp");
                self.emit("ret");

                self.in_function = false;
                self.stack_offset = 0;
                self.var_offsets.clear();
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
                // Check if then branch ends with return
                let then_returns = then
                    .last()
                    .map_or(false, |stmt| matches!(stmt, Stmt::Return { .. }));

                // Generate then branch
                for stmt in then {
                    self.gen_stmt(stmt);
                }

                // Only jump to end if then branch doesn't return
                if else_branch.is_some() && !then_returns {
                    self.emit(&format!("jmp {}", end_label));
                }

                // Generate else branch if it exists
                if let Some(else_stmts) = else_branch {
                    self.emit(&format!("{}:", else_label));

                    // Check if else branch ends with return
                    let else_returns = else_stmts
                        .last()
                        .map_or(false, |stmt| matches!(stmt, Stmt::Return { .. }));

                    for stmt in else_stmts {
                        self.gen_stmt(stmt);
                    }
                }

                // Only generate end label if needed
                let needs_end_label = if let Some(else_stmts) = else_branch {
                    !then_returns
                        || !else_stmts
                            .last()
                            .map_or(false, |stmt| matches!(stmt, Stmt::Return { .. }))
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
            Expr::Const { value } => {
                self.emit(&format!("movq ${}, %rax", value));
            }
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
        }
    }

    fn format_asm(&self) -> String {
        self.output
            .lines()
            .map(|line| {
                let trimmed = line.trim_end();
                if trimmed.ends_with(':') || trimmed.starts_with('.') || trimmed.is_empty() {
                    line.to_string()
                } else {
                    format!("  {}", line)
                }
            })
            .collect::<Vec<_>>()
            .join("\n")
    }
}
