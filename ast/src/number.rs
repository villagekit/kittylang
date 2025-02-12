#[derive(Debug, Copy, Clone, PartialEq)]
struct Number(D128);

impl Number {
    const CONTEXT: decimal::Context = decimal::Context::default();

    fn parse(s: &str) -> Result<Self, decimal::ParseError> {
        let d: D128 = decimal::Decimal::from_str(s, Self::CONTEXT)?;
        Ok(Self(d))
    }
}

impl From<D128> for Number {
    fn from(value: D128) -> Self {
        Number(value)
    }
}
