use kitty_ast::SpannedExpr;
use kitty_meta::Spanned;

#[derive(Clone, Debug)]
pub enum Value<'src> {
    Num(f64),
    Bool(bool),
    Func {
        arg: Spanned<&'src str>,
        env: Scope<'src>,
        body: &'src SpannedExpr<'src>,
    },
}

impl Value<'_> {
    pub fn num(self) -> f64 {
        let Value::Num(x) = self else { panic!() };
        x
    }
}

pub type Scope<'src> = Vec<(Spanned<&'src str>, Value<'src>)>;
