use fastnum::{
    decimal::{Context, Decimal, ParseError},
    D128,
};

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Number(D128);

impl Number {
    const CONTEXT: Context = Context::default();

    pub fn parse(s: &str) -> Result<Self, ParseError> {
        let d: D128 = Decimal::from_str(s, Self::CONTEXT)?;
        Ok(Self(d))
    }
}

impl From<D128> for Number {
    fn from(value: D128) -> Self {
        Number(value)
    }
}

#[cfg(test)]
mod tests {
    use fastnum::dec128;

    use super::*;

    #[test]
    fn it_works() {
        let expected: Result<Number, ParseError> = Ok(dec128!(0.2).into());
        let actual = Number::parse("0.2");
        assert_eq!(expected, actual);
    }
}
