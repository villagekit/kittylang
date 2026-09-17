//! The playground crate is the compiler as a library for a web page. One
//! function, `inspect_json`, takes source text and returns what the
//! compiler sees as JSON: the tokens, the syntax tree and the parse errors.
//! The page in `web/` calls it through the `wasm-bindgen` export
//! `inspect` on every keystroke.

use serde::Serialize;
use wasm_bindgen::prelude::wasm_bindgen;

use kitty_lexer::{lex, Tokens};
use kitty_parser::parse;

/// Lexes and parses `source` and returns the result as one JSON object:
/// `{"tokens": [...], "tokens_text": "...", "tree": "...", "errors": [...]}`.
/// See [`Inspection`] for the fields.
pub fn inspect_json(source: &str) -> String {
    // The value is strings and numbers, which always serialise.
    serde_json::to_string(&inspect(source)).expect("an Inspection serialises")
}

/// Lexes and parses `source` and returns what the compiler sees as data.
pub fn inspect(source: &str) -> Inspection {
    let tokens: Tokens = lex(source).into();
    let tokens_text = tokens.to_string();
    let tokens = tokens
        .iter()
        .map(|token| TokenView {
            kind: format!("{:?}", token.kind),
            start: token.range.start().into(),
            end: token.range.end().into(),
        })
        .collect();
    let parse = parse(source);
    let errors = parse
        .errors
        .iter()
        .map(|error| ErrorView {
            start: error.range().start().into(),
            end: error.range().end().into(),
            message: error.message(),
        })
        .collect();
    Inspection {
        tokens,
        tokens_text,
        tree: format!("{:#?}", parse.tree),
        errors,
    }
}

/// The `wasm-bindgen` export the page calls: [`inspect_json`] by another
/// name, `inspect` in JavaScript.
#[wasm_bindgen(js_name = inspect)]
pub fn inspect_wasm(source: &str) -> String {
    inspect_json(source)
}

/// What the compiler sees in one source text.
#[derive(Debug, Serialize)]
pub struct Inspection {
    /// The tokens in source order, trivia included, as data for colouring
    /// the source.
    pub tokens: Vec<TokenView>,
    /// The same tokens printed the way `kitty lex` prints them, one per
    /// line.
    pub tokens_text: String,
    /// The syntax tree, printed the way `kitty parse` prints it.
    pub tree: String,
    /// The parse errors in source order.
    pub errors: Vec<ErrorView>,
}

/// One token: its kind's name and its byte range in the source.
#[derive(Debug, Serialize)]
pub struct TokenView {
    /// The kind's name as the lexer's snapshot tests print it, for example
    /// `IdentifierValue`.
    pub kind: String,
    /// The byte offset where the token starts.
    pub start: u32,
    /// The byte offset where the token ends.
    pub end: u32,
}

/// One parse error: where it is and what it says.
#[derive(Debug, Serialize)]
pub struct ErrorView {
    /// The byte offset where the error starts.
    pub start: u32,
    /// The byte offset where the error ends. Equal to `start` for a
    /// missing token.
    pub end: u32,
    /// What was expected and, for an unexpected token, what was found.
    pub message: String,
}

#[cfg(test)]
mod tests {
    use expect_test::expect;

    use super::*;

    #[test]
    fn a_program_with_one_error_gives_its_tokens_tree_and_error() {
        let json = inspect_json("fn f() => 1 +\n");
        let value: serde_json::Value = serde_json::from_str(&json).expect("the output is JSON");
        let pretty = serde_json::to_string_pretty(&value).expect("a value prints");
        expect![[r#"
            {
              "errors": [
                {
                  "end": 14,
                  "message": "missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’",
                  "start": 14
                }
              ],
              "tokens": [
                {
                  "end": 2,
                  "kind": "Fn",
                  "start": 0
                },
                {
                  "end": 3,
                  "kind": "Whitespace",
                  "start": 2
                },
                {
                  "end": 4,
                  "kind": "IdentifierValue",
                  "start": 3
                },
                {
                  "end": 5,
                  "kind": "ParenOpen",
                  "start": 4
                },
                {
                  "end": 6,
                  "kind": "ParenClose",
                  "start": 5
                },
                {
                  "end": 7,
                  "kind": "Whitespace",
                  "start": 6
                },
                {
                  "end": 9,
                  "kind": "FatArrow",
                  "start": 7
                },
                {
                  "end": 10,
                  "kind": "Whitespace",
                  "start": 9
                },
                {
                  "end": 11,
                  "kind": "Number",
                  "start": 10
                },
                {
                  "end": 12,
                  "kind": "Whitespace",
                  "start": 11
                },
                {
                  "end": 13,
                  "kind": "Plus",
                  "start": 12
                },
                {
                  "end": 14,
                  "kind": "Newline",
                  "start": 13
                }
              ],
              "tokens_text": "Fn@0..2\nWhitespace@2..3\nIdentifierValue@3..4\nParenOpen@4..5\nParenClose@5..6\nWhitespace@6..7\nFatArrow@7..9\nWhitespace@9..10\nNumber@10..11\nWhitespace@11..12\nPlus@12..13\nNewline@13..14\n",
              "tree": "Module@0..14\n  ModuleLocal@0..14\n    DeclarationFunction@0..14\n      Fn@0..2 \"fn\"\n      Whitespace@2..3 \" \"\n      IdentifierValue@3..4 \"f\"\n      FunctionParamList@4..6\n        ParenOpen@4..5 \"(\"\n        ParenClose@5..6 \")\"\n      Whitespace@6..7 \" \"\n      FatArrow@7..9 \"=>\"\n      Whitespace@9..10 \" \"\n      FunctionBody@10..14\n        ExpressionBinary@10..14\n          ExpressionLiteral@10..11\n            Number@10..11 \"1\"\n          Whitespace@11..12 \" \"\n          Plus@12..13 \"+\"\n          Newline@13..14 \"\\n\"\n          Missing@14..14\n"
            }"#]]
        .assert_eq(&pretty);
    }
}
