use kitty_syntax::{NodeKind, SyntaxNode, SyntaxToken, SyntaxTree, TokenKind};
use text_size::TextRange;

pub trait CstNode: Sized {
    fn cast(syntax: SyntaxNode, tree: &SyntaxTree) -> Option<Self>;
    fn syntax(self) -> SyntaxNode;
    fn text(self, tree: &SyntaxTree) -> &str {
        self.syntax().text(tree)
    }
    fn range(self, tree: &SyntaxTree) -> TextRange {
        self.syntax().range(tree)
    }
}

pub trait CstToken: Sized {
    fn cast(syntax: SyntaxToken, tree: &SyntaxTree) -> Option<Self>;
    fn syntax(self) -> SyntaxToken;
    fn text(self, tree: &SyntaxTree) -> &str {
        self.syntax().text(tree)
    }
    fn range(self, tree: &SyntaxTree) -> TextRange {
        self.syntax().range(tree)
    }
}

macro_rules! define_node {
    ($kind:ident) => {
        #[derive(Clone, Copy)]
        pub struct $kind(SyntaxNode);

        impl CstNode for $kind {
            fn cast(syntax: SyntaxNode, tree: &SyntaxTree) -> Option<Self> {
                if syntax.kind(tree) == NodeKind::$kind {
                    return Some(Self(syntax));
                }
                None
            }

            fn syntax(self) -> SyntaxNode {
                self.0
            }
        }
    };
}

macro_rules! define_compound_node {
	($name:ident, kinds: [$($kind:ident),+]) => {
		#[derive(Clone, Copy)]
		pub enum $name {
			$( $kind($kind), )+
		}

		impl CstNode for $name {
			fn cast(syntax: SyntaxNode, tree: &SyntaxTree) -> Option<Self> {
				match syntax.kind(tree) {
					$( NodeKind::$kind => Some(Self::$kind($kind(syntax))), )+
					_ => None
				}
			}

			fn syntax(self) -> SyntaxNode {
				match self {
					$( Self::$kind($kind(s)) => s, )+
				}
			}
		}
	};
}

macro_rules! define_token {
    ($kind:ident) => {
        #[derive(Clone, Copy)]
        pub struct $kind(SyntaxToken);

        impl CstToken for $kind {
            fn cast(syntax: SyntaxToken, tree: &SyntaxTree) -> Option<Self> {
                if syntax.kind(tree) == TokenKind::$kind {
                    return Some(Self(syntax));
                }
                None
            }

            fn syntax(self) -> SyntaxToken {
                self.0
            }
        }
    };
}

macro_rules! define_compound_token {
	($name:ident, kinds: [$($kind:ident),+]) => {
		#[derive(Clone, Copy)]
		pub enum $name {
			$( $kind($kind), )+
		}

		impl CstToken for $name {
			fn cast(syntax: SyntaxToken, tree: &SyntaxTree) -> Option<Self> {
				match syntax.kind(tree) {
					$( TokenKind::$kind => Some(Self::$kind($kind(syntax))), )+
					_ => None
				}
			}

			fn syntax(self) -> SyntaxToken {
				match self {
					$( Self::$kind($kind(s)) => s, )+
				}
			}
		}
	};
}

define_node!(Source);

impl Source {
    pub fn declarations(self, tree: &SyntaxTree) -> impl Iterator<Item = Decl> + '_ {
        nodes(self, tree)
    }
}

// Expressions
define_node!(IdentifierExpr);
define_node!(LiteralExpr);

define_node!(ParenExpr);
define_node!(BlockExpr);
define_node!(LetExpr);
impl LetExpr {
    pub fn name(self, tree: &SyntaxTree) -> Option<Identifier> {
        token(self, tree)
    }

    pub fn value(self, tree: &SyntaxTree) -> Option<Expr> {
        node(self, tree)
    }
}
define_node!(IfExpr);
define_node!(CallExpr);
define_node!(TupleExpr);
define_node!(UnaryExpr);
impl UnaryExpr {
    pub fn right(self, tree: &SyntaxTree) -> Option<Expr> {
        nodes(self, tree).nth(1)
    }

    pub fn op(self, tree: &SyntaxTree) -> Option<UnaryOperator> {
        token(self, tree)
    }
}
define_node!(BinaryExpr);
impl BinaryExpr {
    pub fn lhs(self, tree: &SyntaxTree) -> Option<Expr> {
        node(self, tree)
    }

    pub fn rhs(self, tree: &SyntaxTree) -> Option<Expr> {
        nodes(self, tree).nth(1)
    }

    pub fn op(self, tree: &SyntaxTree) -> Option<BinaryOperator> {
        token(self, tree)
    }
}
define_node!(GetExpr);
define_compound_node!(Expr, kinds: [IdentifierExpr, LiteralExpr, ParenExpr, BlockExpr, LetExpr, IfExpr, MatchExpr, CallExpr, TupleExpr, UnaryExpr, BinaryExpr, GetExpr]);

// Match
define_node!(MatchExpr);
define_node!(MatchArm);

// Type Path
define_node!(TypeName);
define_node!(TypeGeneric);
define_node!(TypeProjection);
define_node!(TypeAssociation);
define_compound_node!(TypePath, kinds: [TypeName, TypeGeneric, TypeProjection, TypeAssociation]);

// Type Annotation
define_node!(TypeTuple);
define_node!(TypeFunction);
define_node!(TypeTrait);

#[derive(Clone, Copy)]
pub enum TypeAnnotation {
    TypePath(TypePath),
    TypeTuple(TypeTuple),
    TypeFunction(TypeFunction),
    TypeTrait(TypeTrait),
}

impl CstNode for TypeAnnotation {
    fn cast(syntax: SyntaxNode, tree: &SyntaxTree) -> Option<Self> {
        match syntax.kind(tree) {
            NodeKind::TypeName => Some(Self::TypePath(TypePath::TypeName(TypeName(syntax)))),
            NodeKind::TypeGeneric => {
                Some(Self::TypePath(TypePath::TypeGeneric(TypeGeneric(syntax))))
            }
            NodeKind::TypeProjection => Some(Self::TypePath(TypePath::TypeProjection(
                TypeProjection(syntax),
            ))),
            NodeKind::TypeAssociation => Some(Self::TypePath(TypePath::TypeAssociation(
                TypeAssociation(syntax),
            ))),
            NodeKind::TypeTuple => Some(Self::TypeTuple(TypeTuple(syntax))),
            NodeKind::TypeFunction => Some(Self::TypeFunction(TypeFunction(syntax))),
            NodeKind::TypeTrait => Some(Self::TypeTrait(TypeTrait(syntax))),
            _ => None,
        }
    }

    fn syntax(self) -> SyntaxNode {
        match self {
            Self::TypePath(n) => n.syntax(),
            Self::TypeTuple(n) => n.syntax(),
            Self::TypeFunction(n) => n.syntax(),
            Self::TypeTrait(n) => n.syntax(),
        }
    }
}

// Generic Parameters
define_node!(GenericParam);
define_node!(GenericParams);

// Type Arguments
define_node!(GenericArg);
define_node!(GenericArgs);

// Type Bounds
define_node!(TypeBound);
define_node!(TypeBounds);

// Literals
define_token!(Boolean);
define_token!(Number);
define_token!(String);
define_compound_token!(Literal, kinds: [Boolean, Number, String]);

// Identifier
define_token!(Identifier);

// Operators
define_token!(Plus);
define_token!(Minus);
define_token!(Multiply);
define_token!(Divide);
define_token!(GreaterEqual);
define_token!(Greater);
define_token!(LessEqual);
define_token!(Less);
define_token!(EqualEqual);
define_token!(NotEqual);
define_token!(Equal);

define_token!(And);
define_token!(Or);
define_token!(Xor);
define_token!(Not);
define_token!(Rem);

define_compound_token!(BinaryOperator, kinds: [
    Plus,
    Minus,
    Multiply,
    Divide,
    GreaterEqual,
    Greater,
    LessEqual,
    Less,
    NotEqual,
    EqualEqual,
    Equal,
    And,
    Or,
    Xor,
    Not,
    Rem
]);
define_compound_token!(UnaryOperator, kinds: [
    Plus,
    Minus,
    Not
]);

// Patterns.
define_node!(PatternType);
define_node!(PatternLiteral);
define_node!(PatternTuple);
define_node!(PatternConstructor);
define_node!(PatternWildcard);
define_node!(PatternOr);
define_compound_node!(Pattern, kinds: [PatternType, PatternLiteral, PatternTuple, PatternConstructor, PatternWildcard, PatternOr]);

// Top-Level Declarations.
define_node!(TypeDecl);
define_node!(ConstantDecl);

// Function Declarations.
define_node!(FunctionDecl);
define_node!(FunctionParam);
define_node!(FunctionParams);
define_node!(FunctionArg);
define_node!(FunctionArgs);
define_node!(FunctionBody);

// Enum Declarations.
define_node!(EnumDecl);
define_node!(EnumCase);

// Struct Declarations.
define_node!(StructDecl);
define_node!(StructProp);
define_node!(StructFunction);
define_node!(StructConstant);
define_compound_node!(StructItem, kinds: [StructProp, StructFunction, StructConstant]);

// Traits
define_node!(TraitDecl);
define_node!(TraitType);
define_node!(TraitProp);
define_node!(TraitFunction);
define_node!(TraitConstant);
define_compound_node!(TraitItem, kinds: [TraitType, TraitProp, TraitFunction, TraitConstant]);

// Impl
define_node!(ImplTraitDecl);
define_node!(ImplTraitType);
define_node!(ImplTraitProp);
define_node!(ImplTraitFunction);
define_node!(ImplTraitConstant);
define_compound_node!(ImplTraitItem, kinds: [ImplTraitType, ImplTraitProp, ImplTraitFunction, ImplTraitConstant]);

define_compound_node!(Decl, kinds: [TypeDecl, ConstantDecl, FunctionDecl, EnumDecl, StructDecl, TraitDecl, ImplTraitDecl]);

impl StructDecl {
    pub fn name(self, tree: &SyntaxTree) -> Option<Identifier> {
        token(self, tree)
    }

    pub fn properties(self, tree: &SyntaxTree) -> impl Iterator<Item = StructProp> + '_ {
        nodes(self, tree)
    }
}

impl StructProp {
    pub fn name(self, tree: &SyntaxTree) -> Option<Identifier> {
        token(self, tree)
    }

    // e.g. the type annotation or some other child node
    // pub fn ty(self, tree: &SyntaxTree) -> Option<...> {
    //     node(self, tree)
    // }
}

impl FunctionDecl {
    pub fn name(self, tree: &SyntaxTree) -> Option<Identifier> {
        token(self, tree)
    }

    pub fn body(self, tree: &SyntaxTree) -> Option<BlockExpr> {
        node(self, tree)
    }
}

fn node<Parent: CstNode, Child: CstNode>(node: Parent, tree: &SyntaxTree) -> Option<Child> {
    node.syntax()
        .child_nodes(tree)
        .find_map(|c| Child::cast(c, tree))
}

fn token<Parent: CstNode, Child: CstToken>(node: Parent, tree: &SyntaxTree) -> Option<Child> {
    node.syntax()
        .child_tokens(tree)
        .find_map(|c| Child::cast(c, tree))
}

fn nodes<Parent: CstNode, Child: CstNode>(
    node: Parent,
    tree: &SyntaxTree,
) -> impl Iterator<Item = Child> + '_ {
    node.syntax()
        .child_nodes(tree)
        .filter_map(move |c| Child::cast(c, tree))
}

fn tokens<Parent: CstNode, Child: CstToken>(
    node: Parent,
    tree: &SyntaxTree,
) -> impl Iterator<Item = Child> + '_ {
    node.syntax()
        .child_tokens(tree)
        .filter_map(move |c| Child::cast(c, tree))
}
