use kitty_meta::Spanned;

/// A generic type argument.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArg<'src>(TypeAnnotation<'src>);

/// Generic type arguments.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericArgs<'src> {
    positional: Vec<Spanned<TypeAnnotation<'src>>>,
    labelled: Vec<(Spanned<&'src str>, Spanned<TypeAnnotation<'src>>)>
}

/// A generic type parameter.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GenericParam<'src> {
    name: Spanned<&'src str>,
    /// Optional trait bounds.
    bounds: Option<Vec<Spanned<TypeBound<'src>>>>,
    /// An optional default type.
    default: Option<Spanned<TypeAnnotation<'src>>>,
}

/// A type bound (for use in generic parameter declarations).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeBound<'src>(TypeAnnotation<'src>);

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeAnnotation<'src> {
    /// Named types (user-defined or from standard library)
    Identifier(&'src str),

    /// Parametrized types with type arguments (e.g. `List[Number]`, same as `Vec<f64>` in Rust)
    Parametrized {
        base: Box<Spanned<Self>>,
        args: Vec<Spanned<GenericArg<'src>>>,
    },

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

    /// Associated type access (e.g. `Iterator.Item`, same as `Iterator::Item` in Rust)
    Associated {
        trt: Box<Spanned<Self>>,
        typ: Box<Spanned<Self>>,
    },

    /// Trait projection (e.g. `T.[Iterator]`, same as `<T as Iterator>` in Rust)
    Projection {
        typ: Box<Spanned<Self>>,
        trt: Spanned<&'src str>,
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


/// An expression represents an entity which can be evaluated to a value.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Expression<'src> {
    /// Block expression.
    Block(Block<'src>),

    /// Literal boolean.
    Boolean(bool),

    /// Literal string.
    String(&'src str),

    /// Literal number.
    Number(f64),

    /// A named local variable.
    Identifier(&'src str),

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
        args: FnArgs,
    },

    /// Get key operation (`c.z`).
    GetKey {
        container: Box<Spanned<Self>>,
        key: Spanned<String>,
    },

    /// An `if` expression.
    If {
        cond: Box<Spanned<Self>>,
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
    Name(&'src str),
    /// A tuple pattern.
    Tuple(Vec<Spanned<Pat<'src>>>),
    /// A struct pattern, with a path and a list of field patterns.
    Struct {
        path: TypePath<'src>,
        fields: Vec<Spanned<RecordPatField<'src>>>,
        /// `true` if the pattern uses “..” to capture the rest.
        rest: bool,
    },
    /// A literal pattern.
    Literal(Literal<'src>),
    // Additional pattern forms (slice, or-pattern, etc.) can be added.
}

/// A single field in a record (struct) pattern.
#[derive(Debug, Clone)]
pub struct RecordPatField<'src> {
    pub name: Identifier<'src>,
    /// If `None`, then the field is “shorthand” (e.g. `foo`).
    pub pat: Option<Box<Spanned<Pat<'src>>>>,
}

// ────────────────────────────────
// Items (Top‑Level Declarations)
// ────────────────────────────────

/// The kinds of items we support.
#[derive(Debug, Clone)]
pub enum Item<'src> {
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
    // (Other items like Const, Static, TypeAlias, etc. could be added here.)
}

/// Function arguments
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FnArgs<'src> {
    positional: Vec<Spanned<Expression<'src>>>,
    labelled: Vec<(Spanned<&'src str>, Spanned<Expression<'src>>)>
};

/// A named function declaration.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Function<'src> {
    /// The function’s name.
    pub name: Option<&'src str>,
    /// Optional generics.
    pub generics: Option<Vec<Spanned<GenericParam<'src>>>,
    /// The list of parameters.
    pub params: Vec<Spanned<FunctionParam<'src>>>,
    /// Optional return type.
    pub ret: Option<Spanned<Type<'src>>>,
    /// Optional function body (if missing, this is a declaration).
    pub body: Option<Block<'src>>,
}

/// An anonymous lambda function declaration.
#[derive(Debug, Clone)]
pub struct Lambda<'src> {
    /// The function’s name.
    pub ident: Identifier<'src>,
    /// Optional generics.
    pub generics: Option<GenericParamList<'src>>,
    /// The list of parameters.
    pub params: Vec<Spanned<FunctionParam<'src>>>,
    /// Optional return type.
    pub ret: Option<Spanned<Type<'src>>>,
    /// Optional function body (if missing, this is a declaration).
    pub body: Option<Block<'src>>,
}

/// A function parameter.
#[derive(Debug, Clone)]
pub struct FunctionParam<'src> {
    pub pat: Spanned<Pat<'src>>,
    pub ty: Option<Spanned<Type<'src>>>,
}

/// A struct declaration.
#[derive(Debug, Clone)]
pub struct Struct<'src> {
    /// The struct’s name.
    pub ident: Identifier<'src>,
    /// Optional generics.
    pub generics: Option<GenericParamList<'src>>,
    /// The fields.
    pub fields: StructFields<'src>,
}

/// The different forms a struct’s fields can take.
#[derive(Debug, Clone)]
pub enum StructFields<'src> {
    /// A record-like struct: `struct Foo { a: A, b: B }`
    Named(Vec<Spanned<StructField<'src>>>),
    /// A tuple struct: `struct Foo(A, B);`
    Unnamed(Vec<Spanned<StructField<'src>>>),
    /// A unit struct: `struct Foo;`
    Unit,
}

/// A single struct field.
#[derive(Debug, Clone)]
pub struct StructField<'src> {
    /// In a named field, this is the field name.
    pub ident: Option<Identifier<'src>>,
    pub ty: Spanned<Type<'src>>,
}

/// An enum declaration.
#[derive(Debug, Clone)]
pub struct Enum<'src> {
    pub ident: Identifier<'src>,
    pub generics: Option<GenericParamList<'src>>,
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
    pub generics: Option<GenericParamList<'src>>,
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
    pub generics: Option<GenericParamList<'src>>,
    /// Optional “supertraits” (bounds).
    pub bounds: Option<TypeBoundList<'src>>,
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
    Const(ConstItem<'src>),
}

/// A type alias (which may be used for generic associated types).
#[derive(Debug, Clone)]
pub struct TypeAlias<'src> {
    pub ident: Identifier<'src>,
    pub generics: Option<GenericParamList<'src>>,
    pub bounds: Option<TypeBoundList<'src>>,
    /// The aliased type (if defined).
    pub ty: Option<Spanned<Type<'src>>>,
}

/// A constant item.
#[derive(Debug, Clone)]
pub struct ConstItem<'src> {
    pub ident: Identifier<'src>,
    pub ty: Spanned<Type<'src>>,
    pub expr: Option<Spanned<Expr<'src>>>,
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
