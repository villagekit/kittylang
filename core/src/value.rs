use ariadne::{sources, Color, Label, Report, ReportKind};
use chumsky::{input::BorrowInput, pratt::*, prelude::*};
use std::{env, fmt, fs};

#[derive(Clone, Debug)]
pub enum Value<'src> {
    Num(f64),
    Bool(bool),
    Func {
        arg: Spanned<&'src str>,
        env: Scope<'src>,
        body: &'src Spanned<Expr<'src>>,
    },
}

impl Value<'_> {
    pub fn num(self) -> f64 {
        let Value::Num(x) = self else { panic!() };
        x
    }
}
