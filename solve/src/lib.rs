use core::fmt;

use kitty_ast::{Expr, SpannedExpr};
use kitty_meta::{ErrorReport, Span, Spanned};

// Type checker/solver

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct TyVar(usize);

#[derive(Copy, Clone, Debug)]
pub enum TyInfo {
    Unknown,
    Ref(TyVar),
    Num,
    Bool,
    Func(TyVar, TyVar),
}

type SpannedTyInfo = Spanned<TyInfo>;

impl fmt::Display for TyInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TyInfo::Unknown => write!(f, "?"),
            TyInfo::Ref(_) => write!(f, "<ref>"),
            TyInfo::Num => write!(f, "Num"),
            TyInfo::Bool => write!(f, "Bool"),
            TyInfo::Func(_, _) => write!(f, "(_ -> _)"),
        }
    }
}

#[derive(Debug)]
pub enum Ty {
    Num,
    Bool,
    Func(Box<Self>, Box<Self>),
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Ty::Num => write!(f, "Num"),
            Ty::Bool => write!(f, "Bool"),
            Ty::Func(x, y) => write!(f, "{x} -> {y}"),
        }
    }
}

#[derive(Default)]
pub struct Solver {
    vars: Vec<SpannedTyInfo>,
}

impl<'src> Solver {
    fn create_ty(&mut self, info: TyInfo, span: Span) -> TyVar {
        self.vars.push((info, span));
        TyVar(self.vars.len() - 1)
    }

    fn unify(&mut self, a: TyVar, b: TyVar, span: Span) -> Result<(), SolveError<'src>> {
        match (self.vars[a.0].0, self.vars[b.0].0) {
            (TyInfo::Unknown, _) => {
                self.vars[a.0].0 = TyInfo::Ref(b);
                Ok(())
            }
            (_, TyInfo::Unknown) => {
                self.vars[b.0].0 = TyInfo::Ref(a);
                Ok(())
            }
            (TyInfo::Ref(a), _) => {
                self.unify(a, b, span)?;
                Ok(())
            }
            (_, TyInfo::Ref(b)) => {
                self.unify(a, b, span)?;
                Ok(())
            }
            (TyInfo::Num, TyInfo::Num) | (TyInfo::Bool, TyInfo::Bool) => Ok(()),
            (TyInfo::Func(a_i, a_o), TyInfo::Func(b_i, b_o)) => {
                self.unify(b_i, a_i, span)?; // Order swapped: function args are contravariant
                self.unify(a_o, b_o, span)?;
                Ok(())
            }
            (_a_info, _b_info) => Err(SolveError::TypeMismatch {
                span,
                a_ty: self.vars[a.0],
                b_ty: self.vars[b.0],
            }),
        }
    }

    pub fn check(
        &mut self,
        expr: &SpannedExpr<'src>,
        env: &mut Vec<(&'src str, TyVar)>,
    ) -> Result<TyVar, SolveError<'src>> {
        match &expr.0 {
            Expr::Num(_) => Ok(self.create_ty(TyInfo::Num, expr.1)),
            Expr::Bool(_) => Ok(self.create_ty(TyInfo::Bool, expr.1)),
            Expr::Var(name) => env.iter().rev().find(|(n, _)| n == name).map_or_else(
                || {
                    Err(SolveError::NoSuchLocal {
                        name: name.to_string(),
                        expr: expr.clone(),
                    })
                },
                |v| Ok(v.1),
            ),
            Expr::Let { lhs, rhs, then } => {
                let rhs_ty = self.check(rhs, env)?;
                env.push((lhs.0, rhs_ty));
                let out_ty = self.check(then, env)?;
                env.pop();
                Ok(out_ty)
            }
            Expr::Func { arg, body } => {
                let arg_ty = self.create_ty(TyInfo::Unknown, arg.1);
                env.push((arg.0, arg_ty));
                let body_ty = self.check(body, env)?;
                env.pop();
                Ok(self.create_ty(TyInfo::Func(arg_ty, body_ty), expr.1))
            }
            Expr::Apply { func, arg } => {
                let func_ty = self.check(func, env)?;
                let arg_ty = self.check(arg, env)?;
                let out_ty = self.create_ty(TyInfo::Unknown, expr.1);
                let func_req_ty = self.create_ty(TyInfo::Func(arg_ty, out_ty), func.1);
                self.unify(func_req_ty, func_ty, expr.1)?;
                Ok(out_ty)
            }
            Expr::Add(l, r) | Expr::Mul(l, r) => {
                let out_ty = self.create_ty(TyInfo::Num, expr.1);
                let l_ty = self.check(l, env)?;
                self.unify(out_ty, l_ty, expr.1)?;
                let r_ty = self.check(r, env)?;
                self.unify(out_ty, r_ty, expr.1)?;
                Ok(out_ty)
            }
        }
    }

    pub fn solve(&self, var: TyVar) -> Result<Ty, SolveError<'src>> {
        match self.vars[var.0].0 {
            TyInfo::Unknown => Err(SolveError::InferFailure {
                ty: self.vars[var.0],
            }),
            TyInfo::Ref(var) => self.solve(var),
            TyInfo::Num => Ok(Ty::Num),
            TyInfo::Bool => Ok(Ty::Bool),
            TyInfo::Func(i, o) => Ok(Ty::Func(Box::new(self.solve(i)?), Box::new(self.solve(o)?))),
        }
    }
}

#[derive(Debug)]
pub enum SolveError<'src> {
    TypeMismatch {
        span: Span,
        a_ty: SpannedTyInfo,
        b_ty: SpannedTyInfo,
    },
    InferFailure {
        ty: SpannedTyInfo,
    },
    NoSuchLocal {
        name: String,
        expr: SpannedExpr<'src>,
    },
}

impl From<SolveError<'_>> for ErrorReport {
    fn from(value: SolveError) -> Self {
        match value {
            SolveError::TypeMismatch { span, a_ty, b_ty } => ErrorReport {
                span,
                message: format!("Type mismatch between {0} and {1}", a_ty.0, b_ty.0),
                labels: vec![
                    (span, "mismatch occurred here".into()),
                    (a_ty.1, a_ty.0.to_string()),
                    (b_ty.1, b_ty.0.to_string()),
                ],
                notes: vec![],
            },
            SolveError::InferFailure { ty } => ErrorReport {
                span: ty.1,
                message: "Cannot infer type".into(),
                labels: vec![(ty.1, "has unknown type".into())],
                notes: vec![],
            },
            SolveError::NoSuchLocal { name, expr } => ErrorReport {
                span: expr.1,
                message: format!("No such local '{name}'"),
                labels: vec![(expr.1, "not found in scope".into())],
                notes: vec![],
            },
        }
    }
}
