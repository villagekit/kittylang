mod error;
#[cfg(test)]
mod examples;
mod grammar;
mod marker;
mod parser;
mod sink;
mod source;
mod token_set;

use std::fmt;

use kitty_cst::{CstNode, Module};
use kitty_lexer::{lex, Token};
use kitty_syntax::SyntaxTreeBuf;

pub use crate::error::ParseError;
use crate::parser::Parser;
use crate::sink::Sink;

/// Parses a module from source text.
///
/// Every input gives a tree, however malformed. The parser never panics on
/// an input: what it cannot parse becomes `Error` and `Missing` nodes, and
/// the errors come back beside the tree.
pub fn parse(input: &str) -> Parse<Module> {
    let (tree, errors) = parse_grammar(
        |p: &mut Parser<'_>| {
            grammar::module(p);
        },
        input,
    );
    // The module rule completes a `Module` node as the root on every
    // input, so the cast cannot fail.
    let node = Module::cast(tree.root(), &tree).expect("the module rule makes a Module root");
    Parse { tree, node, errors }
}

/// Runs one grammar rule over the whole input and builds its tree.
///
/// The rule must complete exactly one top-level node, which becomes the
/// root of the tree.
pub(crate) fn parse_grammar(
    grammar: impl Fn(&mut Parser<'_>),
    input: &str,
) -> (SyntaxTreeBuf, Vec<ParseError>) {
    let tokens: Vec<Token> = lex(input).collect();
    let (events, errors) = Parser::new(&tokens).parse(grammar);
    let tree = Sink::new(input, &tokens).process(&events);
    (tree, errors)
}

/// The result of a parse: the tree, its typed root and the errors found.
pub struct Parse<N: CstNode> {
    /// The lossless syntax tree: its text is the source.
    pub tree: SyntaxTreeBuf,
    /// The typed view over the tree's root.
    pub node: N,
    /// The parse errors, in source order. Empty when the input is
    /// well-formed.
    pub errors: Vec<ParseError>,
}

impl<N: CstNode> fmt::Debug for Parse<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&render(&self.tree, &self.errors))
    }
}

/// Renders the tree and then each error on its own line, the form the
/// snapshot tests record.
fn render(tree: &SyntaxTreeBuf, errors: &[ParseError]) -> String {
    let tree = format!("{tree:#?}");
    let mut out = tree.strip_suffix('\n').unwrap_or(&tree).to_owned();

    for error in errors {
        out.push('\n');
        out.push_str(&error.to_string());
    }

    out
}

#[cfg(test)]
fn check(input: &str, expected: expect_test::Expect) {
    let result = parse(input);
    expected.assert_eq(&format!("{result:?}"));
}

/// Snapshots one grammar rule's tree and errors, and checks that the
/// rule's node has its typed view `N`. A root that is an `Error` or
/// `Missing` node has no view.
#[cfg(test)]
fn check_grammar<N: CstNode>(
    grammar: impl Fn(&mut Parser<'_>),
    input: &str,
    expected: expect_test::Expect,
) {
    use kitty_syntax::NodeKind;

    let (tree, errors) = parse_grammar(grammar, input);
    expected.assert_eq(&render(&tree, &errors));

    let root = tree.root();
    if !matches!(root.kind(&tree), NodeKind::Error | NodeKind::Missing) {
        assert!(N::cast(root, &tree).is_some(), "the root has no typed view");
    }
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use indoc::indoc;

    use super::*;

    #[test]
    fn empty_input_parses_to_an_empty_module() {
        check("", expect![[r#"Module@0..0"#]]);
    }

    #[test]
    fn trivia_only_input_parses_to_an_empty_module() {
        check(
            "  # a comment\n",
            expect![[r##"
                Module@0..14
                  Whitespace@0..2 "  "
                  Comment@2..13 "# a comment"
                  Newline@13..14 "\n""##]],
        );
    }

    #[test]
    fn lex_example_basic() {
        check(
            indoc! {"
                export fn main() =>
                  let add = fn (x: Number, y: Number) => x + y in
                  let mul = fn (x: Number, y: Number) => x * y in
                  let x = mul(add(5, 42), 2) in
                  add(x, 3.5)
            "},
            expect![[r#"
                Module@0..166
                  ModuleExport@0..166
                    Export@0..6 "export"
                    Whitespace@6..7 " "
                    DeclarationFunction@7..166
                      Fn@7..9 "fn"
                      Whitespace@9..10 " "
                      IdentifierValue@10..14 "main"
                      FunctionParamList@14..16
                        ParenOpen@14..15 "("
                        ParenClose@15..16 ")"
                      Whitespace@16..17 " "
                      FatArrow@17..19 "=>"
                      Newline@19..20 "\n"
                      FunctionBody@20..166
                        ExpressionBlock@20..166
                          Indent@20..22 "  "
                          ExpressionLet@22..165
                            Let@22..25 "let"
                            Whitespace@25..26 " "
                            PatternName@26..29
                              IdentifierValue@26..29 "add"
                            Whitespace@29..30 " "
                            Equal@30..31 "="
                            Whitespace@31..32 " "
                            DeclarationFunction@32..66
                              Fn@32..34 "fn"
                              Whitespace@34..35 " "
                              FunctionParamList@35..57
                                ParenOpen@35..36 "("
                                FunctionParam@36..45
                                  FunctionParamLabel@36..37
                                    IdentifierValue@36..37 "x"
                                  Colon@37..38 ":"
                                  Whitespace@38..39 " "
                                  TypeReference@39..45
                                    IdentifierType@39..45 "Number"
                                Comma@45..46 ","
                                Whitespace@46..47 " "
                                FunctionParam@47..56
                                  FunctionParamLabel@47..48
                                    IdentifierValue@47..48 "y"
                                  Colon@48..49 ":"
                                  Whitespace@49..50 " "
                                  TypeReference@50..56
                                    IdentifierType@50..56 "Number"
                                ParenClose@56..57 ")"
                              Whitespace@57..58 " "
                              FatArrow@58..60 "=>"
                              Whitespace@60..61 " "
                              FunctionBody@61..66
                                ExpressionBinary@61..66
                                  ExpressionReference@61..62
                                    IdentifierValue@61..62 "x"
                                  Whitespace@62..63 " "
                                  Plus@63..64 "+"
                                  Whitespace@64..65 " "
                                  ExpressionReference@65..66
                                    IdentifierValue@65..66 "y"
                            Whitespace@66..67 " "
                            In@67..69 "in"
                            Newline@69..70 "\n"
                            Whitespace@70..72 "  "
                            ExpressionLet@72..165
                              Let@72..75 "let"
                              Whitespace@75..76 " "
                              PatternName@76..79
                                IdentifierValue@76..79 "mul"
                              Whitespace@79..80 " "
                              Equal@80..81 "="
                              Whitespace@81..82 " "
                              DeclarationFunction@82..116
                                Fn@82..84 "fn"
                                Whitespace@84..85 " "
                                FunctionParamList@85..107
                                  ParenOpen@85..86 "("
                                  FunctionParam@86..95
                                    FunctionParamLabel@86..87
                                      IdentifierValue@86..87 "x"
                                    Colon@87..88 ":"
                                    Whitespace@88..89 " "
                                    TypeReference@89..95
                                      IdentifierType@89..95 "Number"
                                  Comma@95..96 ","
                                  Whitespace@96..97 " "
                                  FunctionParam@97..106
                                    FunctionParamLabel@97..98
                                      IdentifierValue@97..98 "y"
                                    Colon@98..99 ":"
                                    Whitespace@99..100 " "
                                    TypeReference@100..106
                                      IdentifierType@100..106 "Number"
                                  ParenClose@106..107 ")"
                                Whitespace@107..108 " "
                                FatArrow@108..110 "=>"
                                Whitespace@110..111 " "
                                FunctionBody@111..116
                                  ExpressionBinary@111..116
                                    ExpressionReference@111..112
                                      IdentifierValue@111..112 "x"
                                    Whitespace@112..113 " "
                                    Multiply@113..114 "*"
                                    Whitespace@114..115 " "
                                    ExpressionReference@115..116
                                      IdentifierValue@115..116 "y"
                              Whitespace@116..117 " "
                              In@117..119 "in"
                              Newline@119..120 "\n"
                              Whitespace@120..122 "  "
                              ExpressionLet@122..165
                                Let@122..125 "let"
                                Whitespace@125..126 " "
                                PatternName@126..127
                                  IdentifierValue@126..127 "x"
                                Whitespace@127..128 " "
                                Equal@128..129 "="
                                Whitespace@129..130 " "
                                ExpressionApply@130..148
                                  ExpressionReference@130..133
                                    IdentifierValue@130..133 "mul"
                                  FunctionArgList@133..148
                                    ParenOpen@133..134 "("
                                    FunctionArgPositional@134..144
                                      ExpressionApply@134..144
                                        ExpressionReference@134..137
                                          IdentifierValue@134..137 "add"
                                        FunctionArgList@137..144
                                          ParenOpen@137..138 "("
                                          FunctionArgPositional@138..139
                                            ExpressionLiteral@138..139
                                              Number@138..139 "5"
                                          Comma@139..140 ","
                                          Whitespace@140..141 " "
                                          FunctionArgPositional@141..143
                                            ExpressionLiteral@141..143
                                              Number@141..143 "42"
                                          ParenClose@143..144 ")"
                                    Comma@144..145 ","
                                    Whitespace@145..146 " "
                                    FunctionArgPositional@146..147
                                      ExpressionLiteral@146..147
                                        Number@146..147 "2"
                                    ParenClose@147..148 ")"
                                Whitespace@148..149 " "
                                In@149..151 "in"
                                Newline@151..152 "\n"
                                Whitespace@152..154 "  "
                                ExpressionApply@154..165
                                  ExpressionReference@154..157
                                    IdentifierValue@154..157 "add"
                                  FunctionArgList@157..165
                                    ParenOpen@157..158 "("
                                    FunctionArgPositional@158..159
                                      ExpressionReference@158..159
                                        IdentifierValue@158..159 "x"
                                    Comma@159..160 ","
                                    Whitespace@160..161 " "
                                    FunctionArgPositional@161..164
                                      ExpressionLiteral@161..164
                                        Number@161..164 "3.5"
                                    ParenClose@164..165 ")"
                          Newline@165..166 "\n"
                          Dedent@166..166 """#]],
        );
    }

    #[test]
    fn lex_example_3d_math() {
        check(
            indoc! {"
                import sqrt from @std/math

                export struct Vector3[N = Number]
                  where
                    N: Default + Add + Mul + Sqrt

                  prop x: N = N.default()

                  prop y: N = N.default()

                  prop z: N = N.default()

                  fn length(self): N =>
                    let Self(x, y, z) = self
                    (x * x + y * y + z * z).sqrt()

                export struct Quaternion
                  prop x: Number = 0

                  prop y: Number = 0

                  prop z: Number = 0

                  prop w: Number = 0
            "},
            expect![[r#"
                Module@0..387
                  ModuleImport@0..26
                    Import@0..6 "import"
                    Whitespace@6..7 " "
                    ImportAliasValue@7..11
                      IdentifierValue@7..11 "sqrt"
                    Whitespace@11..12 " "
                    From@12..16 "from"
                    Whitespace@16..17 " "
                    Package@17..26 "@std/math"
                  Newline@26..27 "\n"
                  Newline@27..28 "\n"
                  ModuleExport@28..275
                    Export@28..34 "export"
                    Whitespace@34..35 " "
                    DeclarationStruct@35..275
                      Struct@35..41 "struct"
                      Whitespace@41..42 " "
                      IdentifierType@42..49 "Vector3"
                      GenericParamList@49..61
                        BracketOpen@49..50 "["
                        GenericParam@50..60
                          IdentifierType@50..51 "N"
                          Whitespace@51..52 " "
                          Equal@52..53 "="
                          Whitespace@53..54 " "
                          TypeReference@54..60
                            IdentifierType@54..60 "Number"
                        BracketClose@60..61 "]"
                      Newline@61..62 "\n"
                      Indent@62..64 "  "
                      GenericWhereClause@64..107
                        Where@64..69 "where"
                        Newline@69..70 "\n"
                        Whitespace@70..72 "  "
                        Indent@72..74 "  "
                        GenericWhereBound@74..103
                          TypeReference@74..75
                            IdentifierType@74..75 "N"
                          Colon@75..76 ":"
                          Whitespace@76..77 " "
                          GenericBoundList@77..103
                            GenericBound@77..84
                              IdentifierType@77..84 "Default"
                            Whitespace@84..85 " "
                            Plus@85..86 "+"
                            Whitespace@86..87 " "
                            GenericBound@87..90
                              IdentifierType@87..90 "Add"
                            Whitespace@90..91 " "
                            Plus@91..92 "+"
                            Whitespace@92..93 " "
                            GenericBound@93..96
                              IdentifierType@93..96 "Mul"
                            Whitespace@96..97 " "
                            Plus@97..98 "+"
                            Whitespace@98..99 " "
                            GenericBound@99..103
                              IdentifierType@99..103 "Sqrt"
                        Newline@103..104 "\n"
                        Newline@104..105 "\n"
                        Whitespace@105..107 "  "
                        Dedent@107..107 ""
                      DeclarationProp@107..130
                        Prop@107..111 "prop"
                        Whitespace@111..112 " "
                        IdentifierValue@112..113 "x"
                        Colon@113..114 ":"
                        Whitespace@114..115 " "
                        TypeReference@115..116
                          IdentifierType@115..116 "N"
                        Whitespace@116..117 " "
                        Equal@117..118 "="
                        Whitespace@118..119 " "
                        ExpressionApply@119..130
                          ExpressionGet@119..128
                            TypeReference@119..120
                              IdentifierType@119..120 "N"
                            Dot@120..121 "."
                            IdentifierValue@121..128 "default"
                          FunctionArgList@128..130
                            ParenOpen@128..129 "("
                            ParenClose@129..130 ")"
                      Newline@130..131 "\n"
                      Newline@131..132 "\n"
                      Whitespace@132..134 "  "
                      DeclarationProp@134..157
                        Prop@134..138 "prop"
                        Whitespace@138..139 " "
                        IdentifierValue@139..140 "y"
                        Colon@140..141 ":"
                        Whitespace@141..142 " "
                        TypeReference@142..143
                          IdentifierType@142..143 "N"
                        Whitespace@143..144 " "
                        Equal@144..145 "="
                        Whitespace@145..146 " "
                        ExpressionApply@146..157
                          ExpressionGet@146..155
                            TypeReference@146..147
                              IdentifierType@146..147 "N"
                            Dot@147..148 "."
                            IdentifierValue@148..155 "default"
                          FunctionArgList@155..157
                            ParenOpen@155..156 "("
                            ParenClose@156..157 ")"
                      Newline@157..158 "\n"
                      Newline@158..159 "\n"
                      Whitespace@159..161 "  "
                      DeclarationProp@161..184
                        Prop@161..165 "prop"
                        Whitespace@165..166 " "
                        IdentifierValue@166..167 "z"
                        Colon@167..168 ":"
                        Whitespace@168..169 " "
                        TypeReference@169..170
                          IdentifierType@169..170 "N"
                        Whitespace@170..171 " "
                        Equal@171..172 "="
                        Whitespace@172..173 " "
                        ExpressionApply@173..184
                          ExpressionGet@173..182
                            TypeReference@173..174
                              IdentifierType@173..174 "N"
                            Dot@174..175 "."
                            IdentifierValue@175..182 "default"
                          FunctionArgList@182..184
                            ParenOpen@182..183 "("
                            ParenClose@183..184 ")"
                      Newline@184..185 "\n"
                      Newline@185..186 "\n"
                      Whitespace@186..188 "  "
                      DeclarationFunction@188..275
                        Fn@188..190 "fn"
                        Whitespace@190..191 " "
                        IdentifierValue@191..197 "length"
                        FunctionParamList@197..203
                          ParenOpen@197..198 "("
                          FunctionParam@198..202
                            FunctionParamLabel@198..202
                              SelfLower@198..202 "self"
                          ParenClose@202..203 ")"
                        Colon@203..204 ":"
                        Whitespace@204..205 " "
                        FunctionReturnType@205..206
                          TypeReference@205..206
                            IdentifierType@205..206 "N"
                        Whitespace@206..207 " "
                        FatArrow@207..209 "=>"
                        Newline@209..210 "\n"
                        Whitespace@210..212 "  "
                        FunctionBody@212..275
                          ExpressionBlock@212..275
                            Indent@212..214 "  "
                            ExpressionLet@214..275
                              Let@214..217 "let"
                              Whitespace@217..218 " "
                              PatternType@218..231
                                TypeReference@218..222
                                  SelfUpper@218..222 "Self"
                                PatternTypeArgList@222..231
                                  ParenOpen@222..223 "("
                                  PatternTypeArgPositional@223..224
                                    IdentifierValue@223..224 "x"
                                  Comma@224..225 ","
                                  Whitespace@225..226 " "
                                  PatternTypeArgPositional@226..227
                                    IdentifierValue@226..227 "y"
                                  Comma@227..228 ","
                                  Whitespace@228..229 " "
                                  PatternTypeArgPositional@229..230
                                    IdentifierValue@229..230 "z"
                                  ParenClose@230..231 ")"
                              Whitespace@231..232 " "
                              Equal@232..233 "="
                              Whitespace@233..234 " "
                              ExpressionApply@234..273
                                ExpressionGet@234..271
                                  ExpressionApply@234..266
                                    ExpressionReference@234..238
                                      SelfLower@234..238 "self"
                                    Newline@238..239 "\n"
                                    Whitespace@239..243 "    "
                                    FunctionArgList@243..266
                                      ParenOpen@243..244 "("
                                      FunctionArgPositional@244..265
                                        ExpressionBinary@244..265
                                          ExpressionBinary@244..257
                                            ExpressionBinary@244..249
                                              ExpressionReference@244..245
                                                IdentifierValue@244..245 "x"
                                              Whitespace@245..246 " "
                                              Multiply@246..247 "*"
                                              Whitespace@247..248 " "
                                              ExpressionReference@248..249
                                                IdentifierValue@248..249 "x"
                                            Whitespace@249..250 " "
                                            Plus@250..251 "+"
                                            Whitespace@251..252 " "
                                            ExpressionBinary@252..257
                                              ExpressionReference@252..253
                                                IdentifierValue@252..253 "y"
                                              Whitespace@253..254 " "
                                              Multiply@254..255 "*"
                                              Whitespace@255..256 " "
                                              ExpressionReference@256..257
                                                IdentifierValue@256..257 "y"
                                          Whitespace@257..258 " "
                                          Plus@258..259 "+"
                                          Whitespace@259..260 " "
                                          ExpressionBinary@260..265
                                            ExpressionReference@260..261
                                              IdentifierValue@260..261 "z"
                                            Whitespace@261..262 " "
                                            Multiply@262..263 "*"
                                            Whitespace@263..264 " "
                                            ExpressionReference@264..265
                                              IdentifierValue@264..265 "z"
                                      ParenClose@265..266 ")"
                                  Dot@266..267 "."
                                  IdentifierValue@267..271 "sqrt"
                                FunctionArgList@271..273
                                  ParenOpen@271..272 "("
                                  ParenClose@272..273 ")"
                              Newline@273..274 "\n"
                              Newline@274..275 "\n"
                              Missing@275..275
                              Missing@275..275
                            Dedent@275..275 ""
                      Dedent@275..275 ""
                  ModuleExport@275..387
                    Export@275..281 "export"
                    Whitespace@281..282 " "
                    DeclarationStruct@282..387
                      Struct@282..288 "struct"
                      Whitespace@288..289 " "
                      IdentifierType@289..299 "Quaternion"
                      Newline@299..300 "\n"
                      Indent@300..302 "  "
                      DeclarationProp@302..320
                        Prop@302..306 "prop"
                        Whitespace@306..307 " "
                        IdentifierValue@307..308 "x"
                        Colon@308..309 ":"
                        Whitespace@309..310 " "
                        TypeReference@310..316
                          IdentifierType@310..316 "Number"
                        Whitespace@316..317 " "
                        Equal@317..318 "="
                        Whitespace@318..319 " "
                        ExpressionLiteral@319..320
                          Number@319..320 "0"
                      Newline@320..321 "\n"
                      Newline@321..322 "\n"
                      Whitespace@322..324 "  "
                      DeclarationProp@324..342
                        Prop@324..328 "prop"
                        Whitespace@328..329 " "
                        IdentifierValue@329..330 "y"
                        Colon@330..331 ":"
                        Whitespace@331..332 " "
                        TypeReference@332..338
                          IdentifierType@332..338 "Number"
                        Whitespace@338..339 " "
                        Equal@339..340 "="
                        Whitespace@340..341 " "
                        ExpressionLiteral@341..342
                          Number@341..342 "0"
                      Newline@342..343 "\n"
                      Newline@343..344 "\n"
                      Whitespace@344..346 "  "
                      DeclarationProp@346..364
                        Prop@346..350 "prop"
                        Whitespace@350..351 " "
                        IdentifierValue@351..352 "z"
                        Colon@352..353 ":"
                        Whitespace@353..354 " "
                        TypeReference@354..360
                          IdentifierType@354..360 "Number"
                        Whitespace@360..361 " "
                        Equal@361..362 "="
                        Whitespace@362..363 " "
                        ExpressionLiteral@363..364
                          Number@363..364 "0"
                      Newline@364..365 "\n"
                      Newline@365..366 "\n"
                      Whitespace@366..368 "  "
                      DeclarationProp@368..386
                        Prop@368..372 "prop"
                        Whitespace@372..373 " "
                        IdentifierValue@373..374 "w"
                        Colon@374..375 ":"
                        Whitespace@375..376 " "
                        TypeReference@376..382
                          IdentifierType@376..382 "Number"
                        Whitespace@382..383 " "
                        Equal@383..384 "="
                        Whitespace@384..385 " "
                        ExpressionLiteral@385..386
                          Number@385..386 "0"
                      Newline@386..387 "\n"
                      Dedent@387..387 ""
                error at 275: missing ‘in’
                error at 275: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’"#]],
        );
    }
}
