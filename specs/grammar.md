# Grammar

Tokens to a syntax tree: the surface syntax, the tree it parses to, and
how the parser recovers from malformed input. The subsystem is the
`kitty-parser` crate over `kitty-syntax`'s node kinds; its entry is
`kitty_parser::parse`, which takes a `&str` and returns a `Parse<Module>`
holding the tree, the typed `Module` view over its root, and the list of
parse errors.

The contract, the citation form and the **Gap:** marker are in
[README.md](README.md). Productions in code blocks are normative.

## Terms

Syntax tree, node kind, parser, event, marker, sink, parse error and
recovery are defined in the glossary
([docs/context.md](../docs/context.md#parsing)), as are attribute, spread
and `let with`. This spec adds:

- **Block**: the tokens between one `Indent` and its matching `Dedent`
  ([lexing](lexing.md#the-indenter)).
- **Type path**: a type name with generic arguments, projections and
  associated-type segments: `Vector3[Length]`, `Self.Output`,
  `T.[Iterator]`.

## Notation

```
"fn"        a token by its spelling
"Number"    a token by its kind, where the kind has no one spelling
Indent      the block tokens, by kind
Name        a rule
[ x ]       optional
{ x }       zero or more
x | y       alternatives
(Kind)      the node kind the rule produces, where the tree has one
```

Trivia (whitespace, newlines, comments) may appear between any two tokens
of a production and are not written.

## Contract

- The parser must produce a tree for every input, however malformed
  (`struct_incomplete_fn`, `parser/src/grammar/declaration.rs`).
- The parser must not panic on any input: a rule's first token is
  guaranteed by the rule that dispatches to it, and everything else is an
  error with recovery (`assembly_lists_its_parse_errors`,
  `parser/src/examples.rs`; `unary_operator_without_an_operand_is_missing`,
  `parser/src/grammar/expression.rs`).
- Empty input must parse to an empty module with no errors
  (`empty_input_parses_to_an_empty_module`, `parser/src/lib.rs`).
- The parser must return on every input (review only).
- The tree must be lossless: every token the lexer yields, trivia
  included, must appear in the tree in source order, so the tree's text is
  the source (`lex_example_basic`, `parser/src/lib.rs`).
- The root must be a `Module` node (`lex_example_basic`,
  `parser/src/lib.rs`).
- A node other than the root must not end in trivia
  (`infix_expression_with_whitespace`, `parser/src/grammar/expression.rs`).
- Every node and token in the tree must carry its text range
  (`lex_example_basic`, `parser/src/lib.rs`).
- Errors must be data, never thrown: `ParseError::Missing { expected,
  offset }` and `ParseError::Unexpected { expected, found, range }`,
  collected in order (`let_expression_missing_equal`,
  `parser/src/grammar/expression.rs`).
- A parse error's `Display` must render as `error at <offset>: missing
  <expected>` or `error at <start>..<end>: expected <expected>[, but
  found <kind>]`, the expected kinds joined by commas and `or`
  (`multiple_expected_did_find`, `parser/src/error.rs`).
- The parser must skip trivia when reading the next token
  (`infix_expression_with_whitespace`, `parser/src/grammar/expression.rs`).
- The parser must not treat a newline as syntax; where a newline ends an
  expression is undecided
  ([design plan d0658cb1](../plans/d0658cb19697-design-newlines-inside-brackets.md))
  (`infix_expression_interspersed_with_newlines`,
  `parser/src/grammar/expression.rs`).

Gap: the `match` arm and `where` bound loops end only at a dedent, and
an arm or a bound consumes nothing when the next token is in its
recovery set or the input has ended, so the parser never returns on
`fn f() => match x` or `fn f() where T: T => 1`; in a release build the
spin grows the event list until the process aborts.

## Recovery

Each rule is given a recovery set: the tokens a caller can continue from.

- When a rule expects a token and the next token is another, then: if the
  next token is in the recovery set or the input has ended, the parser must
  record a `Missing` error at the next token's start, or at the end of the
  input when it has ended (offset zero when the input is empty), and
  produce an empty `Missing` node, consuming nothing
  (`type_decl_missing_identifier`, `parser/src/grammar/declaration.rs`;
  `empty_input_is_a_missing_pattern`, `parser/src/grammar/pattern.rs`);
  otherwise it must record an
  `Unexpected` error, consume the token into an `Error` node, and continue
  (`trait_unknown_item`, `parser/src/grammar/declaration.rs`).
- A rule must produce its node whether or not it recorded an error, so a
  partial construct is still typed in the tree (`struct_incomplete_fn`,
  `parser/src/grammar/declaration.rs`).
- A binary expression whose right operand fails must produce the binary
  node with a `Missing` right operand and stop the operator loop
  (`do_not_operator_if_getting_rhs_failed`,
  `parser/src/grammar/expression.rs`).
- A unary expression whose operand fails must produce the unary node with
  a `Missing` operand (`unary_operator_without_an_operand_is_missing`,
  `parser/src/grammar/expression.rs`).
- In a list of labelled arguments, only the first is known to start with
  a label; one after a comma that does not is an error inside its
  argument node, and the list goes on
  (`labelled_arg_after_a_labelled_arg_recovers`,
  `parser/src/grammar/expression.rs`;
  `generic_labelled_arg_after_a_labelled_arg_recovers`,
  `parser/src/grammar/type.rs`;
  `type_pattern_labelled_arg_after_a_labelled_arg_recovers`,
  `parser/src/grammar/pattern.rs`).
- An error's `expected` list must name the kinds the rule tested since
  the parser last consumed a token, in the order tested, or the one kind
  an `expect` asked for (`call_expression_trailing_comma`,
  `parser/src/grammar/expression.rs`; `trait_unknown_item`,
  `parser/src/grammar/declaration.rs`).

## Module

```
Module        = { ModuleItem }                                   (Module)
ModuleItem    = ModuleImport | ModuleExport | ModuleLocal
ModuleImport  = "import" ImportAlias { "," ImportAlias } "from" "Package" [ Version ]
              | "import" "from" "Package" [ Version ] Indent { ImportAlias } Dedent
                                                                 (ModuleImport)
ImportAlias   = "IdentifierValue" [ ":" "IdentifierValue" ]      (ImportAliasValue)
              | "IdentifierType" [ ":" "IdentifierType" ]        (ImportAliasType)
Version       = ":" "Number"
ModuleExport  = "export" Declaration                             (ModuleExport)
ModuleLocal   = Declaration                                      (ModuleLocal)
```

- A module must consist of imports and declarations only; there is no
  top-level expression (review only).
- The import rule must accept a version suffix after the package and
  keep it in the tree as a version node (review only).
- If a token starts no module item, then the parser must consume it into
  an `Error` node and resume at the next `import`, `export` or
  declaration keyword (review only).
- What a module is, how an import resolves and what the version and the
  alias mean are undecided
  ([design plan a89ddd38](../plans/a89ddd383a16-design-modules-and-imports.md)).

Gap: the import rule does not take the version suffix.

## Attributes

```
Attribute = "@" "IdentifierValue" [ ArgList ]
```

- A declaration may be preceded by attributes, one per line; each must
  attach to the declaration below it
  ([03212e99](../decisions/03212e993893-attributes-replace-metadata-comments.md))
  (review only).
- The arguments must be a [call argument list](#calls-and-arguments),
  so a payload is an ordinary expression (review only).
- The argument list may be omitted when there are no arguments (review
  only).
- If an attribute is followed by no declaration, then the parser must
  record an error and attach the attribute to a `Missing` declaration
  (review only).
- The parser must accept any value identifier as an attribute name; the
  set is the host's, not the compiler's (review only).

Example (non-normative):

```
@label("Seat width")
@range(min = 5, max = 10, step = 5)
prop seat_width: Number
```

Gap: the parser has no attribute rule.

## Declarations

```
Declaration     = { Attribute } ( TypeAlias | Constant | Function | Enum | Struct | Trait | Impl )
TypeAlias       = "type" "IdentifierType" [ GenericParamList ] "=" TypeAnnotation
                                                                 (DeclarationType)
Constant        = "const" "IdentifierValue" [ ":" TypeAnnotation ] [ "=" Expression ]
                                                                 (DeclarationConstant)
Prop            = "prop" "IdentifierValue" [ ":" TypeAnnotation ] [ "=" Expression ]
                                                                 (DeclarationProp)
Struct          = "struct" "IdentifierType" [ GenericParamList ] Indent [ WhereClause ] { StructItem } Dedent
                                                                 (DeclarationStruct)
StructItem      = { Attribute } ( Constant | Function | Prop )
Enum            = "enum" "IdentifierType" [ GenericParamList ] Indent [ WhereClause ] { EnumItem } Dedent
                                                                 (DeclarationEnum)
EnumItem        = { Attribute } ( Constant | Function | EnumCase )
EnumCase        = "case" "IdentifierType" [ ":" TypeAnnotation ]  (EnumCase)
Trait           = "trait" "IdentifierType" [ GenericParamList ] Indent [ WhereClause ] { TraitItem } Dedent
                                                                 (DeclarationTrait)
TraitItem       = { Attribute } ( AssociatedType | Constant | Function | Prop )
AssociatedType  = "type" "IdentifierType" [ GenericParamList ] [ ":" BoundList ] [ "=" TypeAnnotation ]
                                                                 (DeclarationType)
Impl            = "impl" [ GenericParamList ] TypePath "for" TypeAnnotation Indent [ WhereClause ] { ImplItem } Dedent
                                                                 (DeclarationImplTrait)
ImplItem        = { Attribute } ( TypeAlias | Constant | Function | Prop )
```

- A struct, enum, trait or impl body must be a block (`top_struct`,
  `top_enum`, `top_trait`, `top_impl`, `parser/src/grammar/declaration.rs`).
- Where a body has a `where` clause, it must be the body's first line
  (`lex_example_3d_math`, `parser/src/lib.rs`).
- Whether a member may omit its type or its value depends on where it
  stands:

| Member | Module | Struct | Enum | Trait | Impl | Enforced by |
| --- | --- | --- | --- | --- | --- | --- |
| `const` | value required, type optional | value required, type optional | value required, type optional | both optional | value required, type optional | (`top_const_no_type`, `trait_const_decl`, `impl_all_items`, `parser/src/grammar/declaration.rs`) |
| `prop` | none | type required, value optional | none | type required, value optional | value required, type optional | (`top_struct`, `impl_all_items`, `parser/src/grammar/declaration.rs`) |
| `fn` | return type optional, body required | return type optional, body required | return type optional, body required | both optional | return type optional, body required | (`top_trait`, `trait_function_ends_at_its_return_type`, `impl_all_items`, `parser/src/grammar/declaration.rs`) |
| `type` | value required | none | none | bounds and default optional | value required | (`top_type`, `trait_type_decl`, `impl_all_items`, `parser/src/grammar/declaration.rs`) |

- A trait may declare associated types, with bounds and a default; an
  impl supplies them with `type Name = Type`
  ([fb9b8ee8](../decisions/fb9b8ee85b39-associated-types.md))
  (`trait_type_decl`, `impl_all_items`, `parser/src/grammar/declaration.rs`).
- Whether a struct or an enum variant may carry a positional payload,
  `struct Parts(List[PartsItem])` and `case Single(Part)`, is undecided
  ([design plan dd325e81](../plans/dd325e81ad2c-design-tuple-structs-and-parts.md)).
  Today a variant's payload is `case Name: Type` and a struct has no
  positional form.
- Whether a trait may be implemented for a trait, `impl Object3d for
  Assembly`, is undecided
  ([design plan 3738718c](../plans/3738718cde03-design-supertraits-and-blanket-impls.md)).
  The grammar takes any type annotation after `for`.

## Functions

```
Function          = "fn" FunctionName [ GenericParamList ] ParamList [ ":" ReturnType ] [ WhereClause ] [ "=>" Body ]
                                                                 (DeclarationFunction)
FunctionName      = "IdentifierValue" | "from"
ParamList         = "(" [ Param { "," Param } ] ")"                (FunctionParamList)
Param             = ( "IdentifierValue" | "self" ) [ ":" TypeAnnotation ] [ "=" Expression ]
                                                                 (FunctionParam)
ReturnType        = TypeAnnotation                               (FunctionReturnType)
Body              = Expression                                   (FunctionBody)
Lambda            = "fn" [ "IdentifierValue" ] [ GenericParamList ] ParamList [ WhereClause ] "=>" Expression
                                                                 (DeclarationFunction)
```

- A function declaration must carry a parameter list, `()` when it has
  no parameters (`top_impl`, `parser/src/grammar/declaration.rs`). One
  left out still gives a `FunctionParamList` node, holding only the
  `Missing` node or the `Error` node the recovery rule gives, and the
  rule goes on to the `where` clause and the body
  (`function_without_a_parameter_list_is_missing_one`,
  `function_with_a_stray_token_for_its_parameter_list_recovers`,
  `trait_function_without_a_parameter_list_recovers`,
  `parser/src/grammar/declaration.rs`).
- A declaration's function name left out is a `Missing` node, and the
  rule goes on to the parameter list
  (`function_without_a_name_recovers_at_the_parameter_list`,
  `parser/src/grammar/declaration.rs`).
- A function may declare its return type after the parameter list, `fn
  length(self): N`; the colon introduces a type
  ([95cd2585](../decisions/95cd2585f916-colon-introduces-a-type-equals-supplies-a-value.md)).
  The type sits in a `FunctionReturnType` node; the colon stays outside
  it, as the `=>` stays outside the body, while a parameter's colon
  sits inside its `FunctionParam` (`function_with_a_return_type`,
  `parser/src/grammar/declaration.rs`). A type left out after the colon
  is a `Missing` node in the return type, and the rule goes on to the
  `where` clause or the body
  (`function_with_a_return_type_left_out_recovers`,
  `parser/src/grammar/declaration.rs`).
- Where a body follows, `=>` must precede it; the body is an inline
  expression or a block (`top_fn_no_indent`, `top_fn_with_indent`,
  `parser/src/grammar/declaration.rs`). A `=>` left out is an error
  where it should be, and the rule goes on to take what follows as the
  body; when that is a block, its indent is what the error eats, so no
  block forms and the body is the block's first expression alone
  (`function_with_a_return_type_and_no_fat_arrow_recovers`,
  `parser/src/grammar/declaration.rs`).
- In a trait, a function with no body must end at its return type, its
  `where` clause or its parameter list (`top_trait`,
  `trait_function_ends_at_its_return_type`,
  `trait_function_ends_at_its_where_clause`,
  `parser/src/grammar/declaration.rs`).
- A parameter may carry a type and a default value (`top_fn_no_indent`,
  `parser/src/grammar/declaration.rs`; the default review only).
- The parser must accept `from` as a function name, so `fn from(value)`
  declares the `From` trait's method (review only).
- A lambda is a function expression: its name is optional and it has no
  return type (`function_expr`, `lambda_in_an_argument`,
  `parser/src/grammar/expression.rs`).

Gap: the parser rejects `from` as a name.

## Expressions

```
Expression     = Operand { BinaryOperator Operand }               (ExpressionBinary, per operator)
Operand        = { UnaryOperator } Postfix                         (ExpressionUnary, per operator)
BinaryOperator = "or" | "xor" | "and" | "==" | "!=" | "<" | "<=" | ">" | ">=" | "+" | "-" | "*" | "/" | "rem"
UnaryOperator  = "+" | "-" | "not"
Postfix        = Primary { Call | FieldGet }
Call       = ArgList                                             (ExpressionApply)
FieldGet   = "." "IdentifierValue"                               (ExpressionGet)
Primary    = Reference | TypePath [ "." "IdentifierValue" ] | Literal | Tuple | Block
           | Lambda | Let | LetWith | If | Match
Reference  = "IdentifierValue" | "self"                          (ExpressionReference)
Literal    = "Number" | "String"                                 (ExpressionLiteral)
Tuple      = "(" [ Expression { "," Expression } [ "," ] ] ")"   (ExpressionTuple)
Block      = Indent Expression Dedent                            (ExpressionBlock)
```

The productions above are precedence-free; operators bind from loosest
to tightest as the table says, and every binary operator is
left-associative.

| Level | Operators | Enforced by |
| --- | --- | --- |
| 1 | `or` | review only |
| 2 | `xor` | review only |
| 3 | `and` | review only |
| 4 | `==`, `!=` | review only |
| 5 | `<`, `<=`, `>`, `>=` | review only |
| 6 | `+`, `-` | (`left_associative_infix_expression`, `parser/src/grammar/expression.rs`) |
| 7 | `*`, `/`, `rem` | (`infix_expression_with_mixed_binding_power`, `parser/src/grammar/expression.rs`) |
| 8 | prefix `+`, `-`, `not` | (`negation_has_higher_binding_power_than_binary_operators`, `parser/src/grammar/expression.rs`) |
| 9 | call, field access | review only |

- A parenthesised expression must parse as a one-element tuple node
  (`parentheses_affect_precedence`, `parser/src/grammar/expression.rs`).
- A block must hold one expression; a sequence is written as nested
  `let` (`top_fn_with_indent`, `parser/src/grammar/declaration.rs`).
- `True` and `False` must parse as type paths, not literals; there is
  no boolean literal node (review only).
- In expression position, a type path may be followed by `.` and a
  value identifier, which ends the type path and continues as an
  expression: `N.default()`, `Self.regular()`, `Type.Assoc.value`
  ([f2708b12](../decisions/f2708b12e004-value-and-type-identifiers-are-separate.md))
  (review only).
- `<` and `>` are comparison operators only; there is no angle-bracket
  form ([ec7345d9](../decisions/ec7345d92813-brackets-generics-tuples-lists-and-indexing.md))
  (review only).
- `[` opens nothing in expression position: a list is `List(...)` and
  indexing is a call, `list(i)`
  ([ec7345d9](../decisions/ec7345d92813-brackets-generics-tuples-lists-and-indexing.md))
  (review only).

Gap: the parser takes only type segments after a type path in an
expression, and parses `True` and `False` as literals through the
`Boolean` token kind.

## Calls and arguments

```
ArgList    = "(" [ Arg { "," Arg } ] ")"                         (FunctionArgList)
           | "{" [ Arg { "," Arg } ] "}"
           | Indent { Arg } Dedent
Arg        = Expression                                          (FunctionArgPositional)
           | "IdentifierValue" "=" Expression                    (FunctionArgLabelled)
           | "..." Expression
```

- A function or a struct is called with `( )` for positional arguments,
  `{ }` for keyword arguments, or a block of keyword arguments one per
  line ([88117830](../decisions/881178303bc8-call-syntax-positional-keyword-and-indented.md))
  (`function_expr`, `parser/src/grammar/expression.rs`; the `{ }` and
  block forms review only).
- `( )` may also carry keyword arguments, `GridBeam.Z(x = 0, y = 0)`
  ([95cd2585](../decisions/95cd2585f916-colon-introduces-a-type-equals-supplies-a-value.md))
  (review only).
- A keyword argument must be spelled `name = value`
  ([95cd2585](../decisions/95cd2585f916-colon-introduces-a-type-equals-supplies-a-value.md))
  (review only).
- In `( )`, positional arguments must precede keyword arguments (review
  only).
- `{ }` and the block form must take keyword arguments and spreads only
  (review only).
- A spread, `...expr`, may stand where an argument stands in any of the
  three forms (review only).
- Whether a block under a callee may carry positional lines, one
  argument per line, and whether `if` has a block form without `then`
  among them, is undecided
  ([design plan e6a33eab](../plans/e6a33eab19d4-design-positional-arguments-in-indented-blocks.md)).
  Until then a positional line in a block is a parse error.

Example (non-normative): the three spellings of one construction.

```
Self(x = 0, y = 0)
Self { x = 0, y = 0 }
Self
  ...self
  x = 0
```

Gap: the parser spells a keyword argument `name: value`, fires a call
on `(` only, and has no spread node.

## Let

```
Let     = "let" Pattern [ ":" TypeAnnotation ] "=" Expression ( "in" Expression | Expression )
                                                                 (ExpressionLet)
LetWith = "let" "with" Expression Indent { "IdentifierValue" } Dedent Expression
```

- A `let` must bind a pattern to a value and then continue with a body
  expression (`let_expression_type`, `parser/src/grammar/expression.rs`).
- `in` may separate the value from the body on one line; when the next
  token after the value is not `in`, the value ends there and the body is
  the expression that follows, the newline being the implicit `in`
  (`let_expression_type`, `parser/src/grammar/expression.rs`; without `in`
  review only).
- `let with <expr>` followed by a block of value identifiers, one per
  line, must bind each name to the field of that name on the value; no
  `in` follows the block, and the body is the expression that follows
  (review only).

Example (non-normative):

```
let with self
  seat_width
  seat_depth
let x = seat_width + seat_depth
x * 2
```

Gap: the parser requires `in` and has no `let with` rule.

## If

```
If = "if" Expression "then" Expression [ "else" Expression ]      (ExpressionIf)
```

- An `if` must take `then` before its first branch
  (`if_expression_missing_condition`, `parser/src/grammar/expression.rs`);
  a block form without `then` is the open question named under
  [calls](#calls-and-arguments).
- The `else` branch may be omitted (`if_expression_missing_then_body`,
  `parser/src/grammar/expression.rs`).

## Match

```
Match    = "match" Expression Indent { MatchArm } Dedent          (ExpressionMatch)
MatchArm = Pattern "=>" Expression                               (MatchArm)
```

- The arms must be a block, each arm `pattern => expression`
  (`expression_match`, `parser/src/grammar/expression.rs`).

## Patterns

```
Pattern         = Single { "or" Single }                         (PatternOr, when "or" occurs)
Single          = Name | Wildcard | LiteralPattern | TuplePattern | TypePattern
Name            = "IdentifierValue"                               (PatternName)
Wildcard        = "_"                                            (PatternWildcard)
LiteralPattern  = "Number" | "String"                            (PatternLiteral)
TuplePattern    = "(" Pattern { "," Pattern } ")"                 (PatternTuple)
TypePattern     = TypePath [ "(" [ PatternField { "," PatternField } ] ")" | "{" [ PatternField { "," PatternField } ] "}" ]
                                                                 (PatternType)
PatternField    = "IdentifierValue" [ "=" "IdentifierValue" ]    (PatternTypeArgPositional, PatternTypeArgLabelled)
```

- A value identifier in a pattern must bind; a type path must match a
  constructor, so `Some(thing)` binds `thing` and `None` binds nothing
  ([f2708b12](../decisions/f2708b12e004-value-and-type-identifiers-are-separate.md))
  (`pattern_type_positional_args`, `pattern_type_no_args`,
  `parser/src/grammar/pattern.rs`).
- A constructor pattern's named field must use `=`, `let Self { x = a }
  = self` ([95cd2585](../decisions/95cd2585f916-colon-introduces-a-type-equals-supplies-a-value.md))
  (review only).
- In `( )`, a bare name is a positional field; in `{ }`, a bare name is
  shorthand for `name = name`, `let Self { x, y, z } = self` (review
  only).
- In `( )`, positional fields must precede named fields
  (`pattern_type_mixed_arg`, `parser/src/grammar/pattern.rs`).
- `True` and `False` in a pattern are type patterns, as in an expression
  (review only).
- `Self` must be accepted where a type path starts a pattern
  (`self_type_is_a_type_pattern`, `parser/src/grammar/pattern.rs`).
- An or-pattern is spelled with the `or` keyword (review only). Note: no
  decision covers the spelling; this is the grammar as built.

Gap: the parser has no `{ }` constructor pattern, spells a named field
`name: value`, has the `Boolean` token among its literal patterns, and
aborts on a `Self` pattern.

## Types

```
TypeAnnotation   = TypePath | TupleType | FunctionType | ImplTraitType
TypePath         = TypeReference { GenericArgList | Projection | Association }
TypeReference    = "IdentifierType" | "Self"                       (TypeReference)
GenericArgList   = "[" [ GenericArg { "," GenericArg } ] "]"       (GenericArgList, inside TypeGeneric)
GenericArg       = TypeAnnotation                                 (GenericArgPositional)
                 | GenericLabel "=" TypeAnnotation                (GenericArgLabelled)
GenericLabel     = "IdentifierType"                               see the note on labels below
Projection       = ".[" "IdentifierType" "]"                       (TypeProjection)
Association      = "." "IdentifierType"                            (TypeAssociation)
TupleType        = "(" [ TypeAnnotation { "," TypeAnnotation } ] ")"  (TypeTuple)
FunctionType     = "Fn" "(" [ TypeAnnotation { "," TypeAnnotation } ] ")" "->" TypeAnnotation
                                                                 (TypeFunction)
ImplTraitType    = "impl" "IdentifierType"                         (TypeTrait)
```

- Generic arguments must be written in `[ ]`
  ([ec7345d9](../decisions/ec7345d92813-brackets-generics-tuples-lists-and-indexing.md))
  (`type_generic_single_arg`, `parser/src/grammar/type.rs`).
- In a generic argument list, positional arguments must precede labelled
  ones (`generic_arg_mixed`, `parser/src/grammar/type.rs`).
- A labelled generic argument must be spelled `label = Type`
  ([95cd2585](../decisions/95cd2585f916-colon-introduces-a-type-equals-supplies-a-value.md))
  (review only). Whether the label is the parameter's type identifier
  (`Vector3[N = Number]`) or a value identifier (`Map[key = String]`) is
  not settled: the decision's examples spell it both ways. Today the
  parser takes a type identifier.
- A type path may chain generic arguments, projections and associated
  types in any order: `Result[Ok, Err].Error`
  (`type_chain_generic_association`, `parser/src/grammar/type.rs`).
- `()` is the unit type (`type_tuple_happy`, `parser/src/grammar/type.rs`;
  the empty form review only).

Gap: the parser spells a labelled generic argument `Name: Type`.

## Generics and where clauses

```
GenericParamList = "[" [ GenericParam { "," GenericParam } ] "]"  (GenericParamList)
GenericParam     = "IdentifierType" [ ":" BoundList ] [ "=" TypeAnnotation ]  (GenericParam)
BoundList        = Bound { "+" Bound }                            (GenericBoundList)
Bound            = "IdentifierType"                               (GenericBound)
WhereClause      = "where" Indent { WhereBound } Dedent           (GenericWhereClause)
WhereBound       = TypePath ":" BoundList                         (GenericWhereBound)
```

- A generic parameter may carry bounds after `:` and a default after `=`,
  `Vector3[N = Number]` (`generic_param_multiple`,
  `parser/src/grammar/type.rs`).
- A `where` clause must be a block of `Type: Bound + Bound` lines
  (`lex_example_3d_math`, `parser/src/lib.rs`).
- A bound is a trait name; a bound with generic arguments has no
  spelling (`generic_param_single_bound`, `parser/src/grammar/type.rs`).

## Non-guarantees

- The grammar does not fix what a newline means inside a bracket, nor
  where a newline ends an expression outside one.
- The grammar does not check that a name resolves, that a pattern is
  exhaustive or that an attribute name is known; those are the analysis's.
- A tuple pattern with no elements has no spelling; a tuple expression
  `()` does.
- A constructor pattern's field takes a name, not a nested pattern;
  `Some(None)` has no spelling.
- Whether an attribute on an exported declaration precedes or follows
  `export` is not settled; the productions put it after.
- Where a spread stands among the positional and keyword arguments of
  `( )` is not settled.

## References

- [lexing.md](lexing.md): the tokens this grammar consumes.
- Decisions [f2708b12](../decisions/f2708b12e004-value-and-type-identifiers-are-separate.md),
  [88117830](../decisions/881178303bc8-call-syntax-positional-keyword-and-indented.md),
  [95cd2585](../decisions/95cd2585f916-colon-introduces-a-type-equals-supplies-a-value.md),
  [ec7345d9](../decisions/ec7345d92813-brackets-generics-tuples-lists-and-indexing.md),
  [fb9b8ee8](../decisions/fb9b8ee85b39-associated-types.md),
  [03212e99](../decisions/03212e993893-attributes-replace-metadata-comments.md).
- The node kinds are the `NodeKind` enum in `syntax/src/lib.rs`; the
  typed views over them are `kitty-cst`.
- `eventree` builds the tree from the parser's events; the sink's
  handling of trailing trivia is `parser/src/sink.rs`.
