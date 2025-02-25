mod error;
mod grammar;
mod marker;
mod parser;
mod sink;
mod source;
mod token_set;

use kitty_cst::{CstNode, Module};
use kitty_lexer::{lex, Token};
use kitty_syntax::SyntaxTreeBuf;
use std::fmt;

pub use crate::error::ParseError;
use crate::parser::Parser;
use crate::sink::Sink;

pub fn parse(input: &str) -> Parse<Module> {
    parse_grammar(
        |p: &mut Parser<'_>| {
            grammar::module(p);
        },
        input,
    )
}

pub(crate) fn parse_grammar<Node: CstNode>(
    grammar: impl Fn(&mut Parser<'_>),
    input: &str,
) -> Parse<Node> {
    let tokens: Vec<Token> = lex(input).collect();
    let (events, errors) = Parser::new(&tokens).parse(grammar);
    println!("events: {:?}", events);
    let tree = Sink::new(input, &tokens).process(&events);
    let node = Node::cast(tree.root(), &tree).unwrap();
    Parse { tree, node, errors }
}

pub struct Parse<N: CstNode> {
    pub tree: SyntaxTreeBuf,
    pub node: N,
    pub errors: Vec<ParseError>,
}

impl<N: CstNode> fmt::Debug for Parse<N> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tree = format!("{:#?}", self.tree);
        write!(f, "{}", &tree[0..tree.len() - 1])?;

        for error in &self.errors {
            write!(f, "\n{error}")?;
        }

        Ok(())
    }
}

#[cfg(test)]
fn check(input: &str, expected: expect_test::Expect) {
    let grammar = |p: &mut Parser| {
        grammar::module(p);
    };
    check_grammar::<Module>(grammar, input, expected);
}

#[cfg(test)]
fn check_grammar<Node: CstNode>(
    grammar: impl Fn(&mut Parser<'_>),
    input: &str,
    expected: expect_test::Expect,
) {
    let result: Parse<Node> = parse_grammar(grammar, input);
    let actual = format!("{:?}", result);
    expected.assert_eq(&actual);
}

#[cfg(test)]
mod tests {
    use expect_test::expect;
    use indoc::indoc;

    use super::*;

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
                  ModuleImport@0..23
                    Import@0..6 "import"
                    Whitespace@6..7 " "
                    ImportAliasValue@7..11
                      IdentifierValue@7..11 "sqrt"
                    Whitespace@11..12 " "
                    From@12..16 "from"
                    Whitespace@16..17 " "
                    Package@17..23 "@std/m"
                  Error@23..26
                    IdentifierValue@23..26 "ath"
                  Newline@26..27 "\n"
                  Newline@27..28 "\n"
                  ModuleExport@28..209
                    Export@28..34 "export"
                    Whitespace@34..35 " "
                    DeclarationStruct@35..209
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
                        GenericBound@74..75
                          IdentifierType@74..75 "N"
                        GenericBound@75..76
                          Error@75..76
                            Colon@75..76 ":"
                        Whitespace@76..77 " "
                        GenericBound@77..84
                          IdentifierType@77..84 "Default"
                        Whitespace@84..85 " "
                        GenericBound@85..86
                          Error@85..86
                            Plus@85..86 "+"
                        Whitespace@86..87 " "
                        GenericBound@87..90
                          IdentifierType@87..90 "Add"
                        Whitespace@90..91 " "
                        GenericBound@91..92
                          Error@91..92
                            Plus@91..92 "+"
                        Whitespace@92..93 " "
                        GenericBound@93..96
                          IdentifierType@93..96 "Mul"
                        Whitespace@96..97 " "
                        GenericBound@97..98
                          Error@97..98
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
                          TypeAssociation@119..128
                            TypeReference@119..120
                              IdentifierType@119..120 "N"
                            Dot@120..121 "."
                            Error@121..128
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
                          TypeAssociation@146..155
                            TypeReference@146..147
                              IdentifierType@146..147 "N"
                            Dot@147..148 "."
                            Error@148..155
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
                          TypeAssociation@173..182
                            TypeReference@173..174
                              IdentifierType@173..174 "N"
                            Dot@174..175 "."
                            Error@175..182
                              IdentifierValue@175..182 "default"
                          FunctionArgList@182..184
                            ParenOpen@182..183 "("
                            ParenClose@183..184 ")"
                      Newline@184..185 "\n"
                      Newline@185..186 "\n"
                      Whitespace@186..188 "  "
                      DeclarationFunction@188..206
                        Fn@188..190 "fn"
                        Whitespace@190..191 " "
                        IdentifierValue@191..197 "length"
                        FunctionParamList@197..203
                          ParenOpen@197..198 "("
                          FunctionParam@198..202
                            FunctionParamLabel@198..202
                              SelfLower@198..202 "self"
                          ParenClose@202..203 ")"
                        Error@203..204
                          Colon@203..204 ":"
                        Whitespace@204..205 " "
                        FunctionBody@205..206
                          TypeReference@205..206
                            IdentifierType@205..206 "N"
                      Whitespace@206..207 " "
                      Error@207..209
                        FatArrow@207..209 "=>"
                  Newline@209..210 "\n"
                  Whitespace@210..212 "  "
                  Error@212..214
                    Indent@212..214 "  "
                  Error@214..217
                    Let@214..217 "let"
                  Whitespace@217..218 " "
                  Error@218..222
                    SelfUpper@218..222 "Self"
                  Error@222..223
                    ParenOpen@222..223 "("
                  Error@223..224
                    IdentifierValue@223..224 "x"
                  Error@224..225
                    Comma@224..225 ","
                  Whitespace@225..226 " "
                  Error@226..227
                    IdentifierValue@226..227 "y"
                  Error@227..228
                    Comma@227..228 ","
                  Whitespace@228..229 " "
                  Error@229..230
                    IdentifierValue@229..230 "z"
                  Error@230..231
                    ParenClose@230..231 ")"
                  Whitespace@231..232 " "
                  Error@232..233
                    Equal@232..233 "="
                  Whitespace@233..234 " "
                  Error@234..238
                    SelfLower@234..238 "self"
                  Newline@238..239 "\n"
                  Whitespace@239..243 "    "
                  Error@243..244
                    ParenOpen@243..244 "("
                  Error@244..245
                    IdentifierValue@244..245 "x"
                  Whitespace@245..246 " "
                  Error@246..247
                    Multiply@246..247 "*"
                  Whitespace@247..248 " "
                  Error@248..249
                    IdentifierValue@248..249 "x"
                  Whitespace@249..250 " "
                  Error@250..251
                    Plus@250..251 "+"
                  Whitespace@251..252 " "
                  Error@252..253
                    IdentifierValue@252..253 "y"
                  Whitespace@253..254 " "
                  Error@254..255
                    Multiply@254..255 "*"
                  Whitespace@255..256 " "
                  Error@256..257
                    IdentifierValue@256..257 "y"
                  Whitespace@257..258 " "
                  Error@258..259
                    Plus@258..259 "+"
                  Whitespace@259..260 " "
                  Error@260..261
                    IdentifierValue@260..261 "z"
                  Whitespace@261..262 " "
                  Error@262..263
                    Multiply@262..263 "*"
                  Whitespace@263..264 " "
                  Error@264..265
                    IdentifierValue@264..265 "z"
                  Error@265..266
                    ParenClose@265..266 ")"
                  Error@266..267
                    Dot@266..267 "."
                  Error@267..271
                    IdentifierValue@267..271 "sqrt"
                  Error@271..272
                    ParenOpen@271..272 "("
                  Error@272..273
                    ParenClose@272..273 ")"
                  Newline@273..274 "\n"
                  Newline@274..275 "\n"
                  Error@275..275
                    Dedent@275..275 ""
                  Error@275..275
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
                error at 23..26: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
                error at 75..76: expected type-id, but found ‘:’
                error at 85..86: expected type-id, but found ‘+’
                error at 91..92: expected type-id, but found ‘+’
                error at 97..98: expected type-id, but found ‘+’
                error at 121..128: expected type-id, but found value-id
                error at 148..155: expected type-id, but found value-id
                error at 175..182: expected type-id, but found value-id
                error at 203..204: expected ‘=>’, but found ‘:’
                error at 207..209: expected dedent, but found ‘=>’
                error at 212..214: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
                error at 214..217: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘let’
                error at 218..222: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘Self’
                error at 222..223: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
                error at 223..224: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
                error at 224..225: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
                error at 226..227: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
                error at 227..228: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
                error at 229..230: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
                error at 230..231: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
                error at 232..233: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
                error at 234..238: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘self’
                error at 243..244: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
                error at 244..245: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
                error at 246..247: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘*’
                error at 248..249: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
                error at 250..251: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
                error at 252..253: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
                error at 254..255: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘*’
                error at 256..257: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
                error at 258..259: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
                error at 260..261: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
                error at 262..263: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘*’
                error at 264..265: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
                error at 265..266: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
                error at 266..267: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
                error at 267..271: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
                error at 271..272: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
                error at 272..273: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
                error at 275..275: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
                error at 275..275: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent"#]],
        );
    }
}
