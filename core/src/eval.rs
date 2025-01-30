use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::{input::BorrowInput, pratt::*, prelude::*};
use std::{env, fmt, fs};

use crate::parser::Expr;

type Scope<'src> = Vec<(Spanned<&'src str>, Value<'src>)>;

#[derive(Default)]
pub struct Vm<'src> {
    stack: Scope<'src>,
}

impl<'src> Vm<'src> {
    pub fn eval(&mut self, expr: &'src Spanned<Expr<'src>>) -> Value<'src> {
        match &expr.0 {
            Expr::Num(x) => Value::Num(*x),
            Expr::Bool(x) => Value::Bool(*x),
            Expr::Var(var) => self
                .stack
                .iter()
                .rev()
                .find(|(v, _)| v.0 == *var)
                .unwrap()
                .1
                .clone(),
            Expr::Let { lhs, rhs, then } => {
                let rhs = self.eval(rhs);
                self.stack.push((*lhs, rhs));
                let then = self.eval(then);
                self.stack.pop();
                then
            }
            Expr::Func { arg, body } => Value::Func {
                arg: **arg,
                env: self.stack.clone(), // TODO: Only save what's actually needed by the function body
                body,
            },
            Expr::Apply { func, arg } => {
                let func = self.eval(func);
                let arg_val = self.eval(arg);
                let Value::Func { arg, body, mut env } = func else {
                    panic!()
                };
                let old_len = self.stack.len();
                self.stack.append(&mut env);
                self.stack.push((arg, arg_val));
                let out = self.eval(body);
                self.stack.truncate(old_len);
                out
            }
            Expr::Add(x, y) => Value::Num(self.eval(x).num() + self.eval(y).num()),
            Expr::Mul(x, y) => Value::Num(self.eval(x).num() * self.eval(y).num()),
        }
    }
}
