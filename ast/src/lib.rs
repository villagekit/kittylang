use fastnum::U128;
use kitty_meta::Spanned;

/// A generic type argument.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArg<'src>(TypeAnnotation<'src>);

/// Generic type arguments.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArgs<'src> {
    positional: Vec<Spanned<TypeAnnotation<'src>>>,
    labelled: Vec<(Spanned<&'src str>, Option<Spanned<TypeAnnotation<'src>>>)>,
}

/// A generic type parameter.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParam<'src> {
    name: Spanned<&'src str>,
    /// Optional trait bounds.
    bounds: Option<TypeBounds<'src>>,
    /// An optional default type.
    default: Option<Spanned<TypeAnnotation<'src>>>,
}

/// Generic type parameters.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParams<'src>(Vec<Spanned<GenericParam<'src>>>);

/// A type bound (for use in generic parameter declarations).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeBound<'src>(TypeAnnotation<'src>);

/// Type bounds (for use in generic parameter declarations).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeBounds<'src>(Vec<Spanned<TypeBound<'src>>>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypePath<'src> {
    /// Named types (user-defined or from standard library)
    Name(&'src str),

    /// Parametrized types with type arguments (e.g. `List[Number]`, same as `Vec<f64>` in Rust)
    Generic {
        base: Box<Spanned<Self>>,
        args: Vec<Spanned<GenericArg<'src>>>,
    },

    /// Trait projection (e.g. `T.[Iterator]`, same as `<T as Iterator>` in Rust)
    Projection {
        typ: Box<Spanned<Self>>,
        trt: Spanned<&'src str>,
    },

    /// Associated type access (e.g. `Iterator.Item`, same as `Iterator::Item` in Rust)
    Association {
        trt: Box<Spanned<Self>>,
        typ: Box<Spanned<Self>>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeAnnotation<'src> {
    // A qualified type
    Path(TypePath<'src>),

    /// A tuple type.
    Tuple(Vec<Spanned<Self>>),

    /// A function type.
    Function {
        params: Vec<Spanned<Self>>,
        ret: Option<Box<Spanned<Self>>>,
    },

    /// A trait type, like `impl Add`
    Trait {
        bounds: Vec<Spanned<TypeBound<'src>>>,
    },
}

/// Unary operators.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOperator {
    Negate,
    Not,
}

/// Binary operators.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinaryOperator {
    And,
    Or,
    Xor,
    Add,
    Subtract,
    Multiply,
    Divide,
    Rem,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    NotEqual,
}

// A literal primitive.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Literal<'src> {
    /// Literal boolean.
    Boolean(bool),

    /// Literal string.
    String(&'src str),

    /// Literal number.
    Number(U128),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MatchArm<'src> {
    /// The pattern for this arm.
    pub pattern: Spanned<Pattern<'src>>,
    /// The expression for this arm.
    pub expression: Spanned<Expression<'src>>,
}

/// An expression represents an entity which can be evaluated to a value.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expression<'src> {
    /// Block expression.
    Block(Block<'src>),

    /// A named local variable.
    Identifier(&'src str),

    /// A literal primitive.
    Literal(Literal<'src>),

    /// A tuple.
    Tuple(Vec<Spanned<Self>>),

    /// A function definition.
    Function(Function<'src>),

    /// An operation on a single [`Expression`] operand with an [`Operator`]
    Unary {
        right: Box<Spanned<Self>>,
        operator: UnaryOperator,
    },

    /// An operation on two [`Expression`] operands with a an [`Operator`].
    Binary {
        left: Box<Spanned<Self>>,
        right: Box<Spanned<Self>>,
        operator: BinaryOperator,
    },

    /// A function invocation with a list of [`Expression`] parameters.
    Call {
        function: Box<Spanned<Self>>,
        args: FunctionArgs<'src>,
    },

    /// Get key operation (`c.z`).
    GetKey {
        container: Box<Spanned<Self>>,
        key: Spanned<String>,
    },

    /// An `if` expression.
    If {
        condition: Box<Spanned<Self>>,
        then_branch: Box<Spanned<Self>>,
        else_branch: Option<Box<Spanned<Self>>>,
    },

    /// A `match` expression.
    Match {
        expr: Box<Spanned<Self>>,
        arms: Vec<Spanned<MatchArm<'src>>>,
    },
}

/// A block of statements with an optional tail expression.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Block<'src> {
    pub statements: Vec<Spanned<Statement<'src>>>,
    pub tail_expression: Option<Box<Spanned<Expression<'src>>>>,
}

/// Statements.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Statement<'src> {
    /// An expression statement.
    Expression(Expression<'src>),

    /// A `let` declaration.
    Let {
        pattern: Spanned<Pattern<'src>>,
        typ: Option<Spanned<TypeAnnotation<'src>>>,
        init: Box<Spanned<Expression<'src>>>,
    },

    /// A declaration (e.g. a function or struct).
    Declaration(Spanned<Declaration<'src>>),
}

/// Patterns used in let bindings, match arms, etc.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Pattern<'src> {
    /// A type path.
    Type(TypePath<'src>),

    /// A literal pattern.
    Literal(Literal<'src>),

    /// A tuple pattern.
    Tuple(Vec<Spanned<Self>>),

    /// A constructor pattern, for .
    Construtor {
        typ: TypePath<'src>,
        positional: Vec<Spanned<Self>>,
        labelled: Vec<(Spanned<&'src str>, Option<Spanned<Self>>)>,
    },

    /// A wildcard pattern (`_`).
    Wildcard,

    /// An or operator pattern.
    Or {
        left: Box<Spanned<Self>>,
        right: Box<Spanned<Self>>,
    },
}

/// A constant.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Constant<'src> {
    pub name: Spanned<&'src str>,
    pub typ: Spanned<TypeAnnotation<'src>>,
    pub expr: Option<Spanned<Expression<'src>>>,
}

/// A type alias.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeAlias<'src> {
    pub name: Spanned<&'src str>,
    pub generics: Option<GenericParams<'src>>,
    pub bounds: Option<TypeBounds<'src>>,
    pub ty: Spanned<TypePath<'src>>,
}

/// Declarations.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Declaration<'src> {
    /// A function.
    Fn(Function<'src>),
    /// A struct.
    Struct(Struct<'src>),
    /// An enum.
    Enum(Enum<'src>),
    /// An implementation block.
    Impl(Impl<'src>),
    /// A trait.
    Trait(Trait<'src>),
    /// Constant.
    Constant(Constant<'src>),
    /// Type alias.
    TypeAlias(TypeAlias<'src>),
}

/// Function arguments
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionArgs<'src> {
    positional: Vec<Spanned<Expression<'src>>>,
    labelled: Vec<(Spanned<&'src str>, Option<Spanned<Expression<'src>>>)>,
}

/// A function declaration.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function<'src> {
    /// The function’s name (if missing, this is an anonymous function).
    pub name: Option<&'src str>,
    /// Optional generics.
    pub generics: Option<GenericParams<'src>>,
    /// The list of parameters.
    pub params: Vec<Spanned<FunctionParam<'src>>>,
    /// Optional return type.
    pub ret: Option<Spanned<TypeAnnotation<'src>>>,
    /// Optional function body (if missing, this is a declaration).
    pub body: Option<Block<'src>>,
}

/// A function parameter.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionParam<'src> {
    pub pattern: Spanned<Pattern<'src>>,
    pub typ: Option<Spanned<TypeAnnotation<'src>>>,
}

/// A struct declaration.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Struct<'src> {
    /// The struct’s name.
    pub name: Spanned<&'src str>,
    /// Optional generics.
    pub generics: Option<GenericParams<'src>>,
    /// Struct declarations: properties, constants, and methods.
    pub items: Vec<Spanned<StructItem<'src>>>,
}

/// A single struct property.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StructProperty<'src> {
    pub name: Spanned<&'src str>,
    pub typ: Spanned<TypeAnnotation<'src>>,
    pub default: Spanned<Expression<'src>>,
}

/// Items that can appear in a struct declaration.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StructItem<'src> {
    Property(StructProperty<'src>),
    Constant(Constant<'src>),
    Method(Function<'src>),
}

/// An enum declaration.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Enum<'src> {
    /// The enum’s name.
    pub name: Spanned<&'src str>,
    /// Optional generics.
    pub generics: Option<GenericParams<'src>>,
    /// Struct declarations: variants, constants, and methods.
    pub items: Vec<Spanned<EnumItem<'src>>>,
}

/// A single enum variant.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct EnumVariant<'src> {
    pub name: Spanned<&'src str>,
    pub typ: Option<TypeAnnotation<'src>>,
}

/// Items that can appear in an enum declaration.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum EnumItem<'src> {
    Variant(EnumVariant<'src>),
    Constant(Constant<'src>),
    Method(Function<'src>),
}

/// A trait declaration.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Trait<'src> {
    /// The traits’s name.
    pub name: Spanned<&'src str>,
    /// Optional generics.
    pub generics: Option<GenericParams<'src>>,
    /// Optional “supertraits” (bounds).
    pub bounds: Option<TypeBounds<'src>>,
    /// The items defined in the trait:
    pub items: Vec<Spanned<TraitItem<'src>>>,
}

/// A generic associated type for traits.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TraitType<'src> {
    pub name: Spanned<&'src str>,
    pub generics: Option<GenericParams<'src>>,
    pub bounds: Option<TypeBounds<'src>>,
    pub default: Spanned<TypePath<'src>>,
}

/// Items that can appear in a trait declaration.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TraitItem<'src> {
    /// An associated type.
    Type(TraitType<'src>),
    /// An associated constant.
    Constant(Constant<'src>),
    /// A required or provided method.
    Method(Function<'src>),
}

/// An implementation block.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Impl<'src> {
    /// Optional generics.
    pub generics: Option<GenericParams<'src>>,
    /// Trait being implemented
    pub trt: Spanned<TypePath<'src>>,
    /// The type being implemented.
    pub typ: Spanned<TypePath<'src>>,
    /// The items defined in the impl.
    pub items: Vec<Spanned<ImplItem<'src>>>,
}

/// Items that can appear in a trait declaration.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ImplItem<'src> {
    /// An associated type.
    Type(TraitType<'src>),
    /// An associated constant.
    Constant(Constant<'src>),
    /// A provided method.
    Method(Function<'src>),
}
