mod indenter;
mod token;

use logos::{Logos, SpannedIter};
use std::{fmt, vec::Vec};

use crate::indenter::Indenter;
pub use crate::token::{Token, TokenKind};

pub fn lex(source: &str) -> Lexer {
    Lexer::new(source)
}

pub struct Lexer<'src> {
    inner: Indenter<'src, SpannedIter<'src, TokenKind>>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        let tokens = TokenKind::lexer(source).spanned();
        let splitter = Indenter::new(source, tokens);
        Self { inner: splitter }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner.next() {
            Some((Ok(token), span)) => Some(Token::new(token, span)),
            Some((Err(_), span)) => Some(Token::new(TokenKind::Error, span)),
            None => None,
        }
    }
}

struct Tokens(Vec<Token>);

impl From<Lexer<'_>> for Tokens {
    fn from(value: Lexer) -> Self {
        Self(value.collect())
    }
}

impl fmt::Debug for Tokens {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for token in self.0.iter() {
            writeln!(f, "{:?}", token)?;
        }
        Ok(())
    }
}

#[cfg(test)]
fn check(input: &str, expected: expect_test::Expect) {
    let tokens: Tokens = lex(input).into();
    let actual = format!("{:?}", tokens);
    expected.assert_eq(&actual);
}

#[cfg(test)]
mod tests {
    use std::ops::Range;

    use expect_test::expect;
    use indoc::indoc;

    use super::*;

    fn check_tokens(input: &str, expected: Vec<(TokenKind, Range<u32>)>) {
        let actual: Vec<(TokenKind, Range<u32>)> = lex(input)
            .map(|token| (token.kind, token.range.into()))
            .collect();

        assert_eq!(expected, actual);
    }

    #[test]
    fn lex_indent() {
        let input = r"
fn foo()
  fn bar()
    baz
";
        use TokenKind::*;
        let expected = vec![
            (Newline, 0..1),
            (Fn, 1..3),
            (Whitespace, 3..4),
            (Identifier, 4..7),
            (ParenOpen, 7..8),
            (ParenClose, 8..9),
            (Newline, 9..10),
            (Indent, 10..12),
            (Fn, 12..14),
            (Whitespace, 14..15),
            (Identifier, 15..18),
            (ParenOpen, 18..19),
            (ParenClose, 19..20),
            (Newline, 20..21),
            (Whitespace, 21..23),
            (Indent, 23..25),
            (Identifier, 25..28),
            (Newline, 28..29),
            (Dedent, 29..29),
            (Dedent, 29..29),
        ];
        check_tokens(input, expected);
    }

    #[test]
    fn lex_indent_empty_lines() {
        let input = r"
fn foo()

  baz

";
        use TokenKind::*;
        let expected = vec![
            (Newline, 0..1),
            (Fn, 1..3),
            (Whitespace, 3..4),
            (Identifier, 4..7),
            (ParenOpen, 7..8),
            (ParenClose, 8..9),
            (Newline, 9..10),
            (Newline, 10..11),
            (Indent, 11..13),
            (Identifier, 13..16),
            (Newline, 16..17),
            (Newline, 17..18),
            (Dedent, 18..18),
        ];
        check_tokens(input, expected);
    }

    #[test]
    fn lex_indent_2() {
        let input = r"
fn foo()
  baz
foo()
";
        use TokenKind::*;
        let expected = vec![
            (Newline, 0..1),
            (Fn, 1..3),
            (Whitespace, 3..4),
            (Identifier, 4..7),
            (ParenOpen, 7..8),
            (ParenClose, 8..9),
            (Newline, 9..10),
            (Indent, 10..12),
            (Identifier, 12..15),
            (Newline, 15..16),
            (Dedent, 16..16),
            (Identifier, 16..19),
            (ParenOpen, 19..20),
            (ParenClose, 20..21),
            (Newline, 21..22),
        ];
        check_tokens(input, expected);
    }

    #[test]
    fn lex_example_basic() {
        check(
            indoc! {"
                export fn main() =>
                  let add = fn (x, y) => x + y
                  let mul = fn (x, y) => x * y
                  let x = mul(add(5, 42), 2)
                  add(x, 3.5)
            "},
            expect![[r#"
                Export@0..6
                Whitespace@6..7
                Fn@7..9
                Whitespace@9..10
                Identifier@10..14
                ParenOpen@14..15
                ParenClose@15..16
                Whitespace@16..17
                FatArrow@17..19
                Newline@19..20
                Indent@20..22
                Let@22..25
                Whitespace@25..26
                Identifier@26..29
                Whitespace@29..30
                Equal@30..31
                Whitespace@31..32
                Fn@32..34
                Whitespace@34..35
                ParenOpen@35..36
                Identifier@36..37
                Comma@37..38
                Whitespace@38..39
                Identifier@39..40
                ParenClose@40..41
                Whitespace@41..42
                FatArrow@42..44
                Whitespace@44..45
                Identifier@45..46
                Whitespace@46..47
                Plus@47..48
                Whitespace@48..49
                Identifier@49..50
                Newline@50..51
                Whitespace@51..53
                Let@53..56
                Whitespace@56..57
                Identifier@57..60
                Whitespace@60..61
                Equal@61..62
                Whitespace@62..63
                Fn@63..65
                Whitespace@65..66
                ParenOpen@66..67
                Identifier@67..68
                Comma@68..69
                Whitespace@69..70
                Identifier@70..71
                ParenClose@71..72
                Whitespace@72..73
                FatArrow@73..75
                Whitespace@75..76
                Identifier@76..77
                Whitespace@77..78
                Multiply@78..79
                Whitespace@79..80
                Identifier@80..81
                Newline@81..82
                Whitespace@82..84
                Let@84..87
                Whitespace@87..88
                Identifier@88..89
                Whitespace@89..90
                Equal@90..91
                Whitespace@91..92
                Identifier@92..95
                ParenOpen@95..96
                Identifier@96..99
                ParenOpen@99..100
                Number@100..101
                Comma@101..102
                Whitespace@102..103
                Number@103..105
                ParenClose@105..106
                Comma@106..107
                Whitespace@107..108
                Number@108..109
                ParenClose@109..110
                Newline@110..111
                Whitespace@111..113
                Identifier@113..116
                ParenOpen@116..117
                Identifier@117..118
                Comma@118..119
                Whitespace@119..120
                Number@120..123
                ParenClose@123..124
                Newline@124..125
                Dedent@125..125
            "#]],
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
                Import@0..6
                Whitespace@6..7
                Identifier@7..11
                Whitespace@11..12
                From@12..16
                Whitespace@16..17
                Package@17..23
                Identifier@23..26
                Newline@26..27
                Newline@27..28
                Export@28..34
                Whitespace@34..35
                Struct@35..41
                Whitespace@41..42
                Identifier@42..49
                BracketOpen@49..50
                Identifier@50..51
                Whitespace@51..52
                Equal@52..53
                Whitespace@53..54
                Identifier@54..60
                BracketClose@60..61
                Newline@61..62
                Indent@62..64
                Where@64..69
                Newline@69..70
                Whitespace@70..72
                Indent@72..74
                Identifier@74..75
                Colon@75..76
                Whitespace@76..77
                Identifier@77..84
                Whitespace@84..85
                Plus@85..86
                Whitespace@86..87
                Identifier@87..90
                Whitespace@90..91
                Plus@91..92
                Whitespace@92..93
                Identifier@93..96
                Whitespace@96..97
                Plus@97..98
                Whitespace@98..99
                Identifier@99..103
                Newline@103..104
                Newline@104..105
                Whitespace@105..107
                Dedent@107..107
                Prop@107..111
                Whitespace@111..112
                Identifier@112..113
                Colon@113..114
                Whitespace@114..115
                Identifier@115..116
                Whitespace@116..117
                Equal@117..118
                Whitespace@118..119
                Identifier@119..120
                Dot@120..121
                Identifier@121..128
                ParenOpen@128..129
                ParenClose@129..130
                Newline@130..131
                Newline@131..132
                Whitespace@132..134
                Prop@134..138
                Whitespace@138..139
                Identifier@139..140
                Colon@140..141
                Whitespace@141..142
                Identifier@142..143
                Whitespace@143..144
                Equal@144..145
                Whitespace@145..146
                Identifier@146..147
                Dot@147..148
                Identifier@148..155
                ParenOpen@155..156
                ParenClose@156..157
                Newline@157..158
                Newline@158..159
                Whitespace@159..161
                Prop@161..165
                Whitespace@165..166
                Identifier@166..167
                Colon@167..168
                Whitespace@168..169
                Identifier@169..170
                Whitespace@170..171
                Equal@171..172
                Whitespace@172..173
                Identifier@173..174
                Dot@174..175
                Identifier@175..182
                ParenOpen@182..183
                ParenClose@183..184
                Newline@184..185
                Newline@185..186
                Whitespace@186..188
                Fn@188..190
                Whitespace@190..191
                Identifier@191..197
                ParenOpen@197..198
                SelfLower@198..202
                ParenClose@202..203
                Colon@203..204
                Whitespace@204..205
                Identifier@205..206
                Whitespace@206..207
                FatArrow@207..209
                Newline@209..210
                Whitespace@210..212
                Indent@212..214
                Let@214..217
                Whitespace@217..218
                SelfUpper@218..222
                Whitespace@222..223
                BraceOpen@223..224
                Whitespace@224..225
                Identifier@225..226
                Comma@226..227
                Whitespace@227..228
                Identifier@228..229
                Comma@229..230
                Whitespace@230..231
                Identifier@231..232
                Whitespace@232..233
                BraceClose@233..234
                Whitespace@234..235
                Equal@235..236
                Whitespace@236..237
                SelfLower@237..241
                Newline@241..242
                Whitespace@242..246
                ParenOpen@246..247
                Identifier@247..248
                Whitespace@248..249
                Multiply@249..250
                Whitespace@250..251
                Identifier@251..252
                Whitespace@252..253
                Plus@253..254
                Whitespace@254..255
                Identifier@255..256
                Whitespace@256..257
                Multiply@257..258
                Whitespace@258..259
                Identifier@259..260
                Whitespace@260..261
                Plus@261..262
                Whitespace@262..263
                Identifier@263..264
                Whitespace@264..265
                Multiply@265..266
                Whitespace@266..267
                Identifier@267..268
                ParenClose@268..269
                Dot@269..270
                Identifier@270..274
                ParenOpen@274..275
                ParenClose@275..276
                Newline@276..277
                Newline@277..278
                Dedent@278..278
                Dedent@278..278
                Export@278..284
                Whitespace@284..285
                Struct@285..291
                Whitespace@291..292
                Identifier@292..302
                Newline@302..303
                Indent@303..305
                Prop@305..309
                Whitespace@309..310
                Identifier@310..311
                Colon@311..312
                Whitespace@312..313
                Identifier@313..319
                Whitespace@319..320
                Equal@320..321
                Whitespace@321..322
                Number@322..323
                Newline@323..324
                Newline@324..325
                Whitespace@325..327
                Prop@327..331
                Whitespace@331..332
                Identifier@332..333
                Colon@333..334
                Whitespace@334..335
                Identifier@335..341
                Whitespace@341..342
                Equal@342..343
                Whitespace@343..344
                Number@344..345
                Newline@345..346
                Newline@346..347
                Whitespace@347..349
                Prop@349..353
                Whitespace@353..354
                Identifier@354..355
                Colon@355..356
                Whitespace@356..357
                Identifier@357..363
                Whitespace@363..364
                Equal@364..365
                Whitespace@365..366
                Number@366..367
                Newline@367..368
                Newline@368..369
                Whitespace@369..371
                Prop@371..375
                Whitespace@375..376
                Identifier@376..377
                Colon@377..378
                Whitespace@378..379
                Identifier@379..385
                Whitespace@385..386
                Equal@386..387
                Whitespace@387..388
                Number@388..389
                Newline@389..390
                Dedent@390..390
            "#]],
        );
    }
}
