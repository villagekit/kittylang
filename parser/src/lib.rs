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
                  let add = fn (x, y) => x + y in
                  let mul = fn (x, y) => x * y in
                  let x = mul(add(5, 42), 2) in
                  add(x, 3.5)
            "},
            expect![[r#"
                Module@0..134
                  ExportItem@0..134
                    Export@0..6 "export"
                    Whitespace@6..7 " "
                    FunctionDecl@7..134
                      Fn@7..9 "fn"
                      Whitespace@9..10 " "
                      Identifier@10..14 "main"
                      FunctionParamList@14..16
                        ParenOpen@14..15 "("
                        ParenClose@15..16 ")"
                      Whitespace@16..17 " "
                      FatArrow@17..19 "=>"
                      Newline@19..20 "\n"
                      FunctionBody@20..134
                        BlockExpr@20..134
                          Indent@20..22 "  "
                          LetExpr@22..133
                            Let@22..25 "let"
                            Whitespace@25..26 " "
                            Identifier@26..29 "add"
                            Whitespace@29..30 " "
                            Equal@30..31 "="
                            Whitespace@31..32 " "
                            FunctionDecl@32..50
                              Fn@32..34 "fn"
                              Whitespace@34..35 " "
                              FunctionParamList@35..41
                                ParenOpen@35..36 "("
                                FunctionParam@36..37
                                  Identifier@36..37 "x"
                                  Missing@37..37
                                  Missing@37..37
                                Comma@37..38 ","
                                Whitespace@38..39 " "
                                FunctionParam@39..40
                                  Identifier@39..40 "y"
                                  Missing@40..40
                                  Missing@40..40
                                ParenClose@40..41 ")"
                              Whitespace@41..42 " "
                              FatArrow@42..44 "=>"
                              Whitespace@44..45 " "
                              FunctionBody@45..50
                                BinaryExpr@45..50
                                  VariableRef@45..46
                                    Identifier@45..46 "x"
                                  Whitespace@46..47 " "
                                  Plus@47..48 "+"
                                  Whitespace@48..49 " "
                                  VariableRef@49..50
                                    Identifier@49..50 "y"
                            Whitespace@50..51 " "
                            In@51..53 "in"
                            Newline@53..54 "\n"
                            Whitespace@54..56 "  "
                            LetExpr@56..133
                              Let@56..59 "let"
                              Whitespace@59..60 " "
                              Identifier@60..63 "mul"
                              Whitespace@63..64 " "
                              Equal@64..65 "="
                              Whitespace@65..66 " "
                              FunctionDecl@66..84
                                Fn@66..68 "fn"
                                Whitespace@68..69 " "
                                FunctionParamList@69..75
                                  ParenOpen@69..70 "("
                                  FunctionParam@70..71
                                    Identifier@70..71 "x"
                                    Missing@71..71
                                    Missing@71..71
                                  Comma@71..72 ","
                                  Whitespace@72..73 " "
                                  FunctionParam@73..74
                                    Identifier@73..74 "y"
                                    Missing@74..74
                                    Missing@74..74
                                  ParenClose@74..75 ")"
                                Whitespace@75..76 " "
                                FatArrow@76..78 "=>"
                                Whitespace@78..79 " "
                                FunctionBody@79..84
                                  BinaryExpr@79..84
                                    VariableRef@79..80
                                      Identifier@79..80 "x"
                                    Whitespace@80..81 " "
                                    Multiply@81..82 "*"
                                    Whitespace@82..83 " "
                                    VariableRef@83..84
                                      Identifier@83..84 "y"
                              Whitespace@84..85 " "
                              In@85..87 "in"
                              Newline@87..88 "\n"
                              Whitespace@88..90 "  "
                              LetExpr@90..133
                                Let@90..93 "let"
                                Whitespace@93..94 " "
                                Identifier@94..95 "x"
                                Whitespace@95..96 " "
                                Equal@96..97 "="
                                Whitespace@97..98 " "
                                CallExpr@98..116
                                  VariableRef@98..101
                                    Identifier@98..101 "mul"
                                  FunctionArgList@101..116
                                    ParenOpen@101..102 "("
                                    FunctionArgPositional@102..112
                                      CallExpr@102..112
                                        VariableRef@102..105
                                          Identifier@102..105 "add"
                                        FunctionArgList@105..112
                                          ParenOpen@105..106 "("
                                          FunctionArgPositional@106..107
                                            NumberLiteral@106..107
                                              Number@106..107 "5"
                                          Comma@107..108 ","
                                          Whitespace@108..109 " "
                                          FunctionArgPositional@109..111
                                            NumberLiteral@109..111
                                              Number@109..111 "42"
                                          ParenClose@111..112 ")"
                                    Comma@112..113 ","
                                    Whitespace@113..114 " "
                                    FunctionArgPositional@114..115
                                      NumberLiteral@114..115
                                        Number@114..115 "2"
                                    ParenClose@115..116 ")"
                                Whitespace@116..117 " "
                                In@117..119 "in"
                                Newline@119..120 "\n"
                                Whitespace@120..122 "  "
                                CallExpr@122..133
                                  VariableRef@122..125
                                    Identifier@122..125 "add"
                                  FunctionArgList@125..133
                                    ParenOpen@125..126 "("
                                    FunctionArgPositional@126..127
                                      VariableRef@126..127
                                        Identifier@126..127 "x"
                                    Comma@127..128 ","
                                    Whitespace@128..129 " "
                                    FunctionArgPositional@129..132
                                      NumberLiteral@129..132
                                        Number@129..132 "3.5"
                                    ParenClose@132..133 ")"
                          Newline@133..134 "\n"
                          Dedent@134..134 ""
                error at 37: missing ‘:’
                error at 37: missing identifier, ‘Self’, ‘(’, ‘Fn’, or ‘impl’
                error at 40: missing ‘:’
                error at 40: missing identifier, ‘Self’, ‘(’, ‘Fn’, or ‘impl’
                error at 71: missing ‘:’
                error at 71: missing identifier, ‘Self’, ‘(’, ‘Fn’, or ‘impl’
                error at 74: missing ‘:’
                error at 74: missing identifier, ‘Self’, ‘(’, ‘Fn’, or ‘impl’"#]],
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
                    let Self { x, y, z } = self
                    (x * x + y * y + z * z).sqrt()

                export struct Quaternion
                  prop x: Number = 0

                  prop y: Number = 0

                  prop z: Number = 0

                  prop w: Number = 0
            "},
            expect![[r#"
                Module@0..390
                  ImportItem@0..23
                    Import@0..6 "import"
                    Whitespace@6..7 " "
                    ImportAlias@7..11
                      Identifier@7..11 "sqrt"
                    Whitespace@11..12 " "
                    From@12..16 "from"
                    Whitespace@16..17 " "
                    Package@17..23 "@std/m"
                  Error@23..26
                    Identifier@23..26 "ath"
                  Newline@26..27 "\n"
                  Newline@27..28 "\n"
                  ExportItem@28..69
                    Export@28..34 "export"
                    Whitespace@34..35 " "
                    StructDecl@35..69
                      Struct@35..41 "struct"
                      Whitespace@41..42 " "
                      Identifier@42..49 "Vector3"
                      GenericParamList@49..61
                        BracketOpen@49..50 "["
                        GenericParam@50..60
                          Identifier@50..51 "N"
                          Whitespace@51..52 " "
                          Equal@52..53 "="
                          Whitespace@53..54 " "
                          TypeName@54..60
                            Identifier@54..60 "Number"
                        BracketClose@60..61 "]"
                      Newline@61..62 "\n"
                      Indent@62..64 "  "
                      Error@64..69
                        Where@64..69 "where"
                  Newline@69..70 "\n"
                  Whitespace@70..72 "  "
                  Error@72..74
                    Indent@72..74 "  "
                  Error@74..75
                    Identifier@74..75 "N"
                  Error@75..76
                    Colon@75..76 ":"
                  Whitespace@76..77 " "
                  Error@77..84
                    Identifier@77..84 "Default"
                  Whitespace@84..85 " "
                  Error@85..86
                    Plus@85..86 "+"
                  Whitespace@86..87 " "
                  Error@87..90
                    Identifier@87..90 "Add"
                  Whitespace@90..91 " "
                  Error@91..92
                    Plus@91..92 "+"
                  Whitespace@92..93 " "
                  Error@93..96
                    Identifier@93..96 "Mul"
                  Whitespace@96..97 " "
                  Error@97..98
                    Plus@97..98 "+"
                  Whitespace@98..99 " "
                  Error@99..103
                    Identifier@99..103 "Sqrt"
                  Newline@103..104 "\n"
                  Newline@104..105 "\n"
                  Whitespace@105..107 "  "
                  Error@107..107
                    Dedent@107..107 ""
                  Error@107..111
                    Prop@107..111 "prop"
                  Whitespace@111..112 " "
                  Error@112..113
                    Identifier@112..113 "x"
                  Error@113..114
                    Colon@113..114 ":"
                  Whitespace@114..115 " "
                  Error@115..116
                    Identifier@115..116 "N"
                  Whitespace@116..117 " "
                  Error@117..118
                    Equal@117..118 "="
                  Whitespace@118..119 " "
                  Error@119..120
                    Identifier@119..120 "N"
                  Error@120..121
                    Dot@120..121 "."
                  Error@121..128
                    Identifier@121..128 "default"
                  Error@128..129
                    ParenOpen@128..129 "("
                  Error@129..130
                    ParenClose@129..130 ")"
                  Newline@130..131 "\n"
                  Newline@131..132 "\n"
                  Whitespace@132..134 "  "
                  Error@134..138
                    Prop@134..138 "prop"
                  Whitespace@138..139 " "
                  Error@139..140
                    Identifier@139..140 "y"
                  Error@140..141
                    Colon@140..141 ":"
                  Whitespace@141..142 " "
                  Error@142..143
                    Identifier@142..143 "N"
                  Whitespace@143..144 " "
                  Error@144..145
                    Equal@144..145 "="
                  Whitespace@145..146 " "
                  Error@146..147
                    Identifier@146..147 "N"
                  Error@147..148
                    Dot@147..148 "."
                  Error@148..155
                    Identifier@148..155 "default"
                  Error@155..156
                    ParenOpen@155..156 "("
                  Error@156..157
                    ParenClose@156..157 ")"
                  Newline@157..158 "\n"
                  Newline@158..159 "\n"
                  Whitespace@159..161 "  "
                  Error@161..165
                    Prop@161..165 "prop"
                  Whitespace@165..166 " "
                  Error@166..167
                    Identifier@166..167 "z"
                  Error@167..168
                    Colon@167..168 ":"
                  Whitespace@168..169 " "
                  Error@169..170
                    Identifier@169..170 "N"
                  Whitespace@170..171 " "
                  Error@171..172
                    Equal@171..172 "="
                  Whitespace@172..173 " "
                  Error@173..174
                    Identifier@173..174 "N"
                  Error@174..175
                    Dot@174..175 "."
                  Error@175..182
                    Identifier@175..182 "default"
                  Error@182..183
                    ParenOpen@182..183 "("
                  Error@183..184
                    ParenClose@183..184 ")"
                  Newline@184..185 "\n"
                  Newline@185..186 "\n"
                  Whitespace@186..188 "  "
                  LocalItem@188..206
                    FunctionDecl@188..206
                      Fn@188..190 "fn"
                      Whitespace@190..191 " "
                      Identifier@191..197 "length"
                      FunctionParamList@197..203
                        ParenOpen@197..198 "("
                        FunctionParam@198..202
                          SelfLower@198..202 "self"
                        ParenClose@202..203 ")"
                      Error@203..204
                        Colon@203..204 ":"
                      Whitespace@204..205 " "
                      FunctionBody@205..206
                        VariableRef@205..206
                          Identifier@205..206 "N"
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
                  Whitespace@222..223 " "
                  Error@223..224
                    BraceOpen@223..224 "{"
                  Whitespace@224..225 " "
                  Error@225..226
                    Identifier@225..226 "x"
                  Error@226..227
                    Comma@226..227 ","
                  Whitespace@227..228 " "
                  Error@228..229
                    Identifier@228..229 "y"
                  Error@229..230
                    Comma@229..230 ","
                  Whitespace@230..231 " "
                  Error@231..232
                    Identifier@231..232 "z"
                  Whitespace@232..233 " "
                  Error@233..234
                    BraceClose@233..234 "}"
                  Whitespace@234..235 " "
                  Error@235..236
                    Equal@235..236 "="
                  Whitespace@236..237 " "
                  Error@237..241
                    SelfLower@237..241 "self"
                  Newline@241..242 "\n"
                  Whitespace@242..246 "    "
                  Error@246..247
                    ParenOpen@246..247 "("
                  Error@247..248
                    Identifier@247..248 "x"
                  Whitespace@248..249 " "
                  Error@249..250
                    Multiply@249..250 "*"
                  Whitespace@250..251 " "
                  Error@251..252
                    Identifier@251..252 "x"
                  Whitespace@252..253 " "
                  Error@253..254
                    Plus@253..254 "+"
                  Whitespace@254..255 " "
                  Error@255..256
                    Identifier@255..256 "y"
                  Whitespace@256..257 " "
                  Error@257..258
                    Multiply@257..258 "*"
                  Whitespace@258..259 " "
                  Error@259..260
                    Identifier@259..260 "y"
                  Whitespace@260..261 " "
                  Error@261..262
                    Plus@261..262 "+"
                  Whitespace@262..263 " "
                  Error@263..264
                    Identifier@263..264 "z"
                  Whitespace@264..265 " "
                  Error@265..266
                    Multiply@265..266 "*"
                  Whitespace@266..267 " "
                  Error@267..268
                    Identifier@267..268 "z"
                  Error@268..269
                    ParenClose@268..269 ")"
                  Error@269..270
                    Dot@269..270 "."
                  Error@270..274
                    Identifier@270..274 "sqrt"
                  Error@274..275
                    ParenOpen@274..275 "("
                  Error@275..276
                    ParenClose@275..276 ")"
                  Newline@276..277 "\n"
                  Newline@277..278 "\n"
                  Error@278..278
                    Dedent@278..278 ""
                  Error@278..278
                    Dedent@278..278 ""
                  ExportItem@278..390
                    Export@278..284 "export"
                    Whitespace@284..285 " "
                    StructDecl@285..390
                      Struct@285..291 "struct"
                      Whitespace@291..292 " "
                      Identifier@292..302 "Quaternion"
                      Newline@302..303 "\n"
                      Indent@303..305 "  "
                      PropDecl@305..323
                        Prop@305..309 "prop"
                        Whitespace@309..310 " "
                        Identifier@310..311 "x"
                        Colon@311..312 ":"
                        Whitespace@312..313 " "
                        TypeName@313..319
                          Identifier@313..319 "Number"
                        Whitespace@319..320 " "
                        Equal@320..321 "="
                        Whitespace@321..322 " "
                        NumberLiteral@322..323
                          Number@322..323 "0"
                      Newline@323..324 "\n"
                      Newline@324..325 "\n"
                      Whitespace@325..327 "  "
                      PropDecl@327..345
                        Prop@327..331 "prop"
                        Whitespace@331..332 " "
                        Identifier@332..333 "y"
                        Colon@333..334 ":"
                        Whitespace@334..335 " "
                        TypeName@335..341
                          Identifier@335..341 "Number"
                        Whitespace@341..342 " "
                        Equal@342..343 "="
                        Whitespace@343..344 " "
                        NumberLiteral@344..345
                          Number@344..345 "0"
                      Newline@345..346 "\n"
                      Newline@346..347 "\n"
                      Whitespace@347..349 "  "
                      PropDecl@349..367
                        Prop@349..353 "prop"
                        Whitespace@353..354 " "
                        Identifier@354..355 "z"
                        Colon@355..356 ":"
                        Whitespace@356..357 " "
                        TypeName@357..363
                          Identifier@357..363 "Number"
                        Whitespace@363..364 " "
                        Equal@364..365 "="
                        Whitespace@365..366 " "
                        NumberLiteral@366..367
                          Number@366..367 "0"
                      Newline@367..368 "\n"
                      Newline@368..369 "\n"
                      Whitespace@369..371 "  "
                      PropDecl@371..389
                        Prop@371..375 "prop"
                        Whitespace@375..376 " "
                        Identifier@376..377 "w"
                        Colon@377..378 ":"
                        Whitespace@378..379 " "
                        TypeName@379..385
                          Identifier@379..385 "Number"
                        Whitespace@385..386 " "
                        Equal@386..387 "="
                        Whitespace@387..388 " "
                        NumberLiteral@388..389
                          Number@388..389 "0"
                      Newline@389..390 "\n"
                      Dedent@390..390 ""
                error at 23..26: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 64..69: expected dedent, but found ‘where’
                error at 72..74: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
                error at 74..75: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 75..76: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
                error at 77..84: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 85..86: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
                error at 87..90: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 91..92: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
                error at 93..96: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 97..98: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
                error at 99..103: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 107..107: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
                error at 107..111: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘prop’
                error at 112..113: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 113..114: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
                error at 115..116: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 117..118: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
                error at 119..120: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 120..121: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
                error at 121..128: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 128..129: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
                error at 129..130: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
                error at 134..138: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘prop’
                error at 139..140: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 140..141: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
                error at 142..143: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 144..145: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
                error at 146..147: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 147..148: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
                error at 148..155: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 155..156: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
                error at 156..157: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
                error at 161..165: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘prop’
                error at 166..167: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 167..168: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
                error at 169..170: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 171..172: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
                error at 173..174: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 174..175: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
                error at 175..182: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 182..183: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
                error at 183..184: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
                error at 203..204: expected ‘=>’, but found ‘:’
                error at 207..209: expected ‘(’, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=>’
                error at 212..214: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
                error at 214..217: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘let’
                error at 218..222: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘Self’
                error at 223..224: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘{’
                error at 225..226: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 226..227: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
                error at 228..229: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 229..230: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
                error at 231..232: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 233..234: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘}’
                error at 235..236: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
                error at 237..241: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘self’
                error at 246..247: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
                error at 247..248: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 249..250: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘*’
                error at 251..252: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 253..254: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
                error at 255..256: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 257..258: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘*’
                error at 259..260: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 261..262: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
                error at 263..264: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 265..266: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘*’
                error at 267..268: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 268..269: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
                error at 269..270: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
                error at 270..274: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found identifier
                error at 274..275: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
                error at 275..276: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
                error at 278..278: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
                error at 278..278: expected ‘import’, ‘export’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent"#]],
        );
    }
}
