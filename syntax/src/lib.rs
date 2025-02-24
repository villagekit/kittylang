use std::mem;

pub use kitty_lexer::TokenKind;

pub type SyntaxBuilder = eventree::SyntaxBuilder<TreeConfig>;
pub type SyntaxElement = eventree::SyntaxElement<TreeConfig>;
pub type SyntaxNode = eventree::SyntaxNode<TreeConfig>;
pub type SyntaxToken = eventree::SyntaxToken<TreeConfig>;
pub type SyntaxTree = eventree::SyntaxTree<TreeConfig>;
pub type SyntaxTreeBuf = eventree::SyntaxTreeBuf<TreeConfig>;
pub type Event = eventree::Event<TreeConfig>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TreeConfig {}

unsafe impl eventree::TreeConfig for TreeConfig {
    type NodeKind = NodeKind;
    type TokenKind = TokenKind;

    fn node_kind_to_raw(node_kind: Self::NodeKind) -> u16 {
        node_kind as u16
    }

    fn token_kind_to_raw(token_kind: Self::TokenKind) -> u16 {
        token_kind as u16
    }

    unsafe fn token_kind_from_raw(raw: u16) -> Self::TokenKind {
        mem::transmute(raw as u8)
    }

    unsafe fn node_kind_from_raw(raw: u16) -> Self::NodeKind {
        mem::transmute(raw as u8)
    }
}

/// Represents a group of tokens or other nodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum NodeKind {
    // Resilient syntax trees
    Error,
    Missing,

    // Identifier
    ConstantName,
    ConstantReference,
    VariableName,
    VariableReference,
    TypeName,
    TypeReference,
    TraitName,
    TraitReference,

    // Module
    Module,
    ModuleImport,
    ImportAliasConstant,
    ImportAliasVariable,
    ImportAliasType,
    ModuleExport,
    ModuleLocal,

    // Expression
    ExpressionLiteral,
    LiteralBoolean,
    LiteralNumber,
    LiteralString,
    ExpressionBlock,
    ExpressionFunction,
    ExpressionLet,
    ExpressionIf,
    ExpressionMatch,
    MatchArm,
    ExpressionApply,
    ExpressionTuple,
    ExpressionUnary,
    ExpressionBinary,
    ExpressionGet,

    // Type Path
    TypeGeneric,
    TypeProjection,
    TypeAssociation,

    // Type Annotation
    TypeTuple,
    TypeFunction,
    TypeTrait,

    // Generic Parameters
    GenericParam,
    GenericParamList,

    // Type Arguments
    GenericArgPositional,
    GenericArgLabelled,
    GenericArgList,

    // Type Bounds
    GenericBound,
    GenericBoundList,

    // Where Clauses
    GenericWhereClause,
    GenericWhereBound,

    // Operators.
    UnaryOperator,
    BinaryOperator,

    // Patterns.
    Pattern,
    PatternOr,
    PatternWildcard,
    PatternLiteral,
    PatternTuple,
    PatternType,
    PatternFieldList,
    PatternFieldPositional,
    PatternFieldLabelled,
    PatternVariable,

    // Top-Level Declarations
    DeclarationType,
    DeclarationConstant,

    // Function Declarations
    DeclarationFunction,
    FunctionParam,
    FunctionParamList,
    FunctionArgPositional,
    FunctionArgLabelled,
    FunctionArgList,
    FunctionBody,

    // Enum Declarations
    DeclarationEnum,
    EnumCase,

    // Struct Declarations
    DeclarationStruct,
    DeclarationProp,

    // Traits
    DeclarationTrait,

    // Impl
    DeclarationImplTrait,
}
