use kitty_meta::Spanned;

pub struct Identifier<'src>(&'src str);

/// A single segment of a path (for example, the “foo” in `Foo.Bar`).
#[derive(Debug, Clone)]
pub struct PathSegment<'src>(Spanned<Identifier<'src>>, Option<GenericArgList<'src>>);

/// A (possibly multi‐segment) path.
#[derive(Debug, Clone)]
pub struct Path<'src>(Vec<Spanned<PathSegment<'src>>>);

/// A generic type argument.
pub struct GenericArg<'src>(TypeAnnotation<'src>);

/// A list of generic type arguments.
pub struct GenericArgList<'src>(Vec<Spanned<GenericArg<'src>>>);

/// A type bound (for use in generic parameter declarations).
#[derive(Debug, Clone)]
pub struct TypeBound<'src>(TypeAnnotation<'src>);

/// A list of type bounds.
pub struct TypeBoundList<'src>(Vec<Spanned<TypeBound<'src>>>);

/// A generic type parameter.
#[derive(Debug, Clone)]
pub struct GenericParam<'src> {
    identifier: Spanned<Identifier<'src>>,
    /// Optional trait bounds.
    bounds: Option<TypeBoundList<'src>>,
    /// An optional default type.
    default: Option<Spanned<TypeAnnotation<'src>>>,
}

/// A list of generic type parameters.
pub type GenericParamList<'src> = Vec<Spanned<GenericParam<'src>>>;

// TODO where clauses

/// A user annotation to describe a type.
#[derive(Debug, Clone)]
pub enum TypeAnnotation<'src> {
    /// A path type, like `Foo` or `Iterator.Item`.
    Path(Path<'src>),
    /// A tuple type.
    Tuple(Vec<Spanned<Type<'src>>>),
    /// A function type.
    Function {
        params: Vec<Spanned<Type<'src>>>,
        ret: Option<Box<Spanned<Type<'src>>>>,
    },
    /// A trait type, like `impl Add`
    Trait {
        bounds: TypeBoundList<'src>
    }
}

/// Literals.
#[derive(Debug, Clone)]
pub enum Literal<'src> {
    /// Boolean literal.
    Boolean(bool),
    /// Number literal.
    Number(&'src str),
    /// String literal.
    String(&'src str),
}

/// Unary operators.
#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Negate,
    Not,
}

/// Binary operators.
#[derive(Debug, Clone)]
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

/// Expressions.
#[derive(Debug, Clone)]
pub enum Expression<'src> {
    /// A literal expression.
    Literal(Literal<'src>),
    /// A variable or constant reference (a path).
    Path(Path<'src>),
    /// A block expression.
    Block(Block<'src>),
    /// A function lambda
    Function {
        args: Vec<Spanned<Identifier<'src>>>,
        body: Box<Spanned<Self>
    },
    /// A function call.
    Call {
        func: Box<Spanned<Self>>,
        args: Vec<Spanned<Self>>,
    },
    /// A binary expression.
    Binary {
        operator: BinaryOperator,
        lhs: Box<Spanned<Self>>,
        rhs: Box<Spanned<Self>>,
    },
    /// A binary expression.
    Binary {
        operator: BinaryOperator,
        lhs: Box<Spanned<Self>>,
        rhs: Box<Spanned<Self>>,
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
    // You can add additional expression forms (loops, closures, etc).
}

/// A block of statements with an optional tail expression.
#[derive(Debug, Clone)]
pub struct Block<'src> {
    pub stmts: Vec<Spanned<Stmt<'src>>>,
    pub expr: Option<Box<Spanned<Expr<'src>>>>,
}

/// Statements.
#[derive(Debug, Clone)]
pub enum Stmt<'src> {
    /// An expression statement.
    Expr(Expr<'src>),
    /// A `let` declaration.
    Let {
        pat: Spanned<Pat<'src>>,
        ty: Option<Spanned<Type<'src>>>,
        init: Box<Spanned<Expr<'src>>>,
    },
    /// An item declaration (e.g. a function or struct).
    Item(Spanned<Item<'src>>),
}

// ────────────────────────────────
// Patterns
// ────────────────────────────────

/// Patterns used in let bindings, match arms, etc.
#[derive(Debug, Clone)]
pub enum Pat<'src> {
    /// An identifier, with an optional “@” pattern.
    Ident(Identifier<'src>, Option<Box<Spanned<Pat<'src>>>>),
    /// A wildcard: `_`.
    Wildcard,
    /// A tuple pattern.
    Tuple(Vec<Spanned<Pat<'src>>>),
    /// A struct pattern, with a path and a list of field patterns.
    Struct {
        path: Path<'src>,
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

/// A function declaration.
#[derive(Debug, Clone)]
pub struct Function<'src> {
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

/// A function declaration.
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
