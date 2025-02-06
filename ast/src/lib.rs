use kitty_meta::Spanned;

/// A generic type argument.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArg<'src>(TypeAnnotation<'src>);

/// Generic type arguments.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArgs<'src> {
    positional: Vec<Spanned<TypeAnnotation<'src>>>,
    labelled: Vec<(Spanned<&'src str>, Option<Spanned<TypeAnnotation<'src>>>)>
}

/// A generic type parameter.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParam<'src> {
    name: Spanned<&'src str>,
    /// Optional trait bounds.
    bounds: Option<TypeBounds>,
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
        bounds: Vec<Spanned<TypeBound<'src>>>
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
    NotEqual
}

// A literal primitive.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Literal<'src> {
    /// Literal boolean.
    Boolean(bool),

    /// Literal string.
    String(&'src str),

    /// Literal number.
    Number(f64),
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
    Function(Function),

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
        args: FunctionArgs,
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
    /// An item declaration (e.g. a function or struct).
    Declaration(Spanned<Declaration<'src>>),
}

/// Patterns used in let bindings, match arms, etc.
#[derive(Debug, Clone)]
pub enum Pattern<'src> {
    /// A named identifier.
    Identifier(&'src str),

    /// A literal pattern.
    Literal(Literal<'src>),

    /// A tuple pattern.
    Tuple(Vec<Spanned<Self>>),

    /// A struct pattern, with a path and a list of field patterns.
    Struct {
        typ: TypePath<'src>,
        positional: Vec<Spanned<Self>>,
        labelled: Vec<(Spanned<&'src str>, Option<Spanned<Self>>)>
    },

    /// A wildcard pattern (`_`).
    Wildcard,

    /// An or operator pattern.
    Or {
        left: Spanned<Self>,
        right: Spanned<Self>,
    }
}

/// Declarations.
#[derive(Debug, Clone)]
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
    labelled: Vec<(Spanned<&'src str>, Option<Spanned<Expression<'src>>>)>
}

/// A named function declaration.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function<'src> {
    /// The function’s name (if missing, this is an anonymous function).
    pub name: Option<&'src str>,
    /// Optional generics.
    pub generics: Option<GenericParams<'src>>,
    /// The list of parameters.
    pub params: Vec<Spanned<FunctionParam<'src>>>,
    /// Optional return type.
    pub ret: Option<Spanned<Type<'src>>>,
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
    pub declarations: Vec<Spanned<StructItem<'src>>,
}

/// Items that can appear in a struct.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum StructItem {
    Property(Property<'src>),
    Constant(Constant<'src>),
    Method(Function<'src>),
}

/// A single struct property.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Property<'src> {
    pub name: Spanned<&'src str>,
    pub typ: Spanned<TypeAnnotation<'src>>,
    pub default: Spanned<Expression<'src>>,
}

/// An enum declaration.
#[derive(Debug, Clone)]
pub struct Enum<'src> {
    pub name: <'src>,
    pub generics: Option<GenericParams<'src>>,
    pub variants: Vec<Spanned<EnumVariant<'src>>>,
}

/// An enum variant.
#[derive(Debug, Clone)]
pub struct EnumVariant<'src> {
    pub ident: Identifier<'src>,
    /// The fields can be record-like, tuple-like, or omitted (for a unit variant).
    pub fields: Option<StructFields<'src>>,
    /// Optional discriminant (e.g. `= 42`).
    pub discriminant: Option<Spanned<Expr<'src>>>,
}

/// An implementation block.
#[derive(Debug, Clone)]
pub struct Impl<'src> {
    /// Optional generics.
    pub generics: Option<GenericParams<'src>>,
    /// If present, this is an “impl of a trait for …”
    pub trait_path: Option<Path<'src>>,
    /// The type being implemented.
    pub self_ty: Spanned<Type<'src>>,
    /// The items (methods, associated types, etc.) defined in the impl.
    pub items: Vec<Spanned<Item<'src>>>,
}

/// A trait declaration.
#[derive(Debug, Clone)]
pub struct Trait<'src> {
    pub ident: Identifier<'src>,
    pub generics: Option<GenericParams<'src>>,
    /// Optional “supertraits” (bounds).
    pub bounds: Option<TypeBounds<'src>>,
    /// The items defined in the trait.
    pub items: Vec<Spanned<TraitItem<'src>>>,
}

/// Items that can appear in a trait.
#[derive(Debug, Clone)]
pub enum TraitItem<'src> {
    /// A required or provided method.
    Fn(Function<'src>),
    /// An associated type.
    TypeAlias(TypeAlias<'src>),
    /// An associated constant.
    Const(Const<'src>),
}

/// A type alias (which may be used for generic associated types).
#[derive(Debug, Clone)]
pub struct TypeAlias<'src> {
    pub name: Spanned<&'src str>,
    pub generics: Option<GenericParams<'src>>,
    pub bounds: Option<TypeBounds<'src>>,
    /// The aliased type (if defined).
    pub ty: Option<Spanned<Type<'src>>>,
}

/// A constant item.
#[derive(Debug, Clone)]
pub struct Constant<'src> {
    pub name: Spanned<&'src str>,
    pub typ: Spanned<TypeAnnotation<'src>>,
    pub expr: Option<Spanned<Expression<'src>>>,
}

// ────────────────────────────────
// Match Arms
// ────────────────────────────────

/// A single arm of a match expression.
#[derive(Debug, Clone)]
pub struct MatchArm<'src> {
    /// The pattern.
    pub pat: Spanned<Pat<'src>>,
    /// An optional guard: `if ...`
    pub guard: Option<Spanned<Expr<'src>>>,
    /// The expression for this arm.
    pub expr: Spanned<Expr<'src>>,
}
}
