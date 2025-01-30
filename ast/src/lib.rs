use kitty_meta::Spanned;

#[derive(Clone, Debug)]
pub enum Expr<'src> {
    Var(&'src str),
    Num(f64),
    Bool(bool),
    Add(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Mul(Box<Spanned<Self>>, Box<Spanned<Self>>),
    Let {
        lhs: Spanned<&'src str>,
        rhs: Box<Spanned<Self>>,
        then: Box<Spanned<Self>>,
    },
    Apply {
        func: Box<Spanned<Self>>,
        arg: Box<Spanned<Self>>,
    },
    Func {
        arg: Box<Spanned<&'src str>>,
        body: Box<Spanned<Self>>,
    },
}

pub type SpannedExpr<'src> = Spanned<Expr<'src>>;
