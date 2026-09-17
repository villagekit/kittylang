mod declaration;
mod expression;
mod function;
mod module;
mod pattern;
mod r#type;

use kitty_syntax::TokenKind;

pub(crate) use module::module;

/// The tokens that name a function or a field: a value identifier, or
/// `from`, a keyword here so `impl From` can declare and reach its method.
pub(crate) const NAME_FIRST: [TokenKind; 2] = [TokenKind::IdentifierValue, TokenKind::From];
