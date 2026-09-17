//! The examples test: every program in `examples/` parsed end to end, with
//! the parse errors it produces recorded per file.
//!
//! The expectations are the fixture guard for the grammar. Each grammar
//! change that is meant to shrink them is reviewed and then recorded with
//! `UPDATE_EXPECT=1`. The six files are named here on purpose, so a new
//! example is added to this list by hand.

use expect_test::{expect, Expect};

use crate::parse;

/// Parses `source` and renders each parse error on its own line, through
/// `ParseError`'s `Display`.
fn check_errors(source: &str, expected: Expect) {
    let parsed = parse(source);
    let actual: String = parsed
        .errors
        .iter()
        .map(|error| format!("{error}\n"))
        .collect();
    expected.assert_eq(&actual);
}

#[test]
fn three_d_math_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/3d-math.kitty"),
        expect![[r#"
            error at 278: missing ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, ‘in’, ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’
        "#]],
    );
}

#[test]
fn three_d_object_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/3d-object.kitty"),
        expect![[r#"
            error at 295..296: expected ‘=’, but found ‘:’
            error at 317..318: expected ‘=’, but found ‘.’
            error at 348..349: expected ‘=’, but found ‘.’
            error at 379..380: expected ‘=’, but found ‘.’
            error at 443..445: expected ‘=>’, but found indent
            error at 467..468: expected ‘=’, but found ‘:’
            error at 509..510: expected ‘=’, but found ‘:’
            error at 543..544: expected ‘=’, but found ‘:’
            error at 564..564: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 738..739: expected ‘=’, but found ‘:’
        "#]],
    );
}

#[test]
fn units_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/units.kitty"),
        expect![[r#"
            error at 228..229: expected ‘=’, but found ‘:’
        "#]],
    );
}

#[test]
fn assembly_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/assembly.kitty"),
        expect![[r#"
            error at 95..96: expected dedent, but found ‘(’
            error at 106..107: expected ‘for’, but found ‘)’
            error at 110..114: expected type-id, ‘Self’, ‘(’, ‘Fn’, or ‘impl’, but found ‘case’
            error at 115..123: expected indent, but found type-id
            error at 123..124: expected dedent, but found ‘(’
            error at 137..138: expected ‘for’, but found ‘)’
            error at 140..140: expected type-id, ‘Self’, ‘(’, ‘Fn’, or ‘impl’, but found dedent
            error at 140: missing indent
            error at 140: missing dedent
            error at 175..176: expected dedent, but found ‘(’
            error at 176..180: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 180..181: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 184..188: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘case’
            error at 189..195: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 195..196: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 196..200: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘Self’
            error at 200..201: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 204..208: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘case’
            error at 209..213: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 215..215: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 234..235: expected indent, but found ‘(’
            error at 235..239: expected dedent, but found type-id
            error at 239..240: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 240..249: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 249..250: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 250..251: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 285: missing ‘[’ or ‘(’
            error at 321: missing indent
            error at 321: missing dedent
        "#]],
    );
}

#[test]
fn chair_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/chair.kitty"),
        expect![[r#"
            error at 194..195: expected ‘=’, but found ‘:’
            error at 202..203: expected ‘=’, but found ‘:’
            error at 212..213: expected ‘=’, but found ‘:’
            error at 279..280: expected ‘=’, but found ‘:’
            error at 287..288: expected ‘=’, but found ‘:’
            error at 424..425: expected ‘=’, but found ‘:’
            error at 432..433: expected ‘=’, but found ‘:’
            error at 685..686: expected ‘=’, but found ‘:’
            error at 693..694: expected ‘=’, but found ‘:’
            error at 1230..1232: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘in’
            error at 1511..1520: expected value-id, ‘self’, or ‘...’, but found type-id
            error at 1533..1534: expected ‘=’, but found ‘:’
            error at 1535..1536: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘[’
            error at 1536..1537: expected value-id, ‘self’, or ‘...’, but found number
            error at 1537..1538: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘,’
            error at 1549..1550: expected ‘=’, but found ‘]’
            error at 1560..1561: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘:’
            error at 1562..1563: expected value-id, ‘self’, or ‘...’, but found ‘[’
            error at 1581..1582: expected ‘=’, but found ‘,’
            error at 1599..1600: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘]’
            error at 1610..1611: expected ‘=’, but found ‘:’
            error at 1616..1622: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id
            error at 1630..1632: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘if’
            error at 1661..1670: expected value-id, ‘self’, or ‘...’, but found type-id
            error at 1685..1686: expected ‘=’, but found ‘:’
            error at 1687..1688: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘[’
            error at 1688..1689: expected value-id, ‘self’, or ‘...’, but found number
            error at 1689..1690: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘,’
            error at 1701..1702: expected ‘=’, but found ‘]’
            error at 1714..1715: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘:’
            error at 1727..1728: expected ‘=’, but found ‘-’
            error at 1742..1743: expected ‘=’, but found ‘:’
            error at 1744..1745: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘[’
            error at 1757..1758: expected ‘=’, but found ‘+’
            error at 1760..1761: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘,’
            error at 1774..1775: expected ‘=’, but found ‘+’
            error at 1791..1792: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘]’
            error at 1806..1807: expected ‘=’, but found ‘:’
            error at 1808..1809: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘error’
            error at 1812..1813: expected ‘=’, but found ‘error’
            error at 1821: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’
            error at 1821..1829: expected ‘then’, but found type-id
            error at 1829..1830: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘.’
            error at 1830..1831: expected ‘else’, ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id
            error at 1841..1842: expected ‘=’, but found ‘:’
            error at 1854..1855: expected ‘=’, but found ‘:’
            error at 1867..1868: expected ‘=’, but found ‘:’
            error at 1869..1870: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘[’
            error at 1870..1871: expected value-id, ‘self’, or ‘...’, but found number
            error at 1871..1872: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘,’
            error at 1884..1885: expected ‘=’, but found ‘]’
            error at 1893: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’
            error at 1893..1901: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id
            error at 1913..1914: expected ‘=’, but found ‘:’
            error at 1939..1940: expected ‘=’, but found ‘:’
            error at 1952..1953: expected ‘=’, but found ‘:’
            error at 1954..1955: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘[’
            error at 1955..1956: expected value-id, ‘self’, or ‘...’, but found number
            error at 1956..1957: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘,’
            error at 1969..1970: expected ‘=’, but found ‘]’
            error at 1978: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’
            error at 1978..1986: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id
            error at 1998..1999: expected ‘=’, but found ‘:’
            error at 2011..2012: expected ‘=’, but found ‘:’
            error at 2037..2038: expected ‘=’, but found ‘:’
            error at 2039..2040: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘[’
            error at 2040..2041: expected value-id, ‘self’, or ‘...’, but found number
            error at 2041..2042: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘,’
            error at 2060..2061: expected ‘=’, but found ‘]’
            error at 2069: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’
            error at 2069..2077: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id
            error at 2089..2090: expected ‘=’, but found ‘:’
            error at 2115..2116: expected ‘=’, but found ‘:’
            error at 2141..2142: expected ‘=’, but found ‘:’
            error at 2143..2144: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘[’
            error at 2144..2145: expected value-id, ‘self’, or ‘...’, but found number
            error at 2145..2146: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘,’
            error at 2164..2165: expected ‘=’, but found ‘]’
            error at 2173: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’
            error at 2173..2181: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id
            error at 2193..2194: expected ‘=’, but found ‘:’
            error at 2195..2196: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘[’
            error at 2196..2197: expected value-id, ‘self’, or ‘...’, but found number
            error at 2197..2198: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘,’
            error at 2209..2210: expected ‘=’, but found ‘]’
            error at 2220..2221: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘:’
            error at 2222..2223: expected value-id, ‘self’, or ‘...’, but found number
            error at 2233..2234: expected ‘=’, but found ‘:’
            error at 2258..2266: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id
            error at 2278..2279: expected ‘=’, but found ‘:’
            error at 2280..2281: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘[’
            error at 2281..2282: expected value-id, ‘self’, or ‘...’, but found number
            error at 2282..2283: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘,’
            error at 2294..2295: expected ‘=’, but found ‘]’
            error at 2305..2306: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘:’
            error at 2318..2319: expected ‘=’, but found ‘-’
            error at 2331..2332: expected ‘=’, but found ‘:’
            error at 2356..2364: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id
            error at 2376..2377: expected ‘=’, but found ‘:’
            error at 2389..2390: expected ‘=’, but found ‘:’
            error at 2391..2392: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘[’
            error at 2392..2393: expected value-id, ‘self’, or ‘...’, but found number
            error at 2393..2394: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘,’
            error at 2405..2406: expected ‘=’, but found ‘]’
            error at 2416..2417: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘:’
            error at 2430..2431: expected ‘=’, but found ‘-’
            error at 2441..2449: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found type-id
            error at 2461..2462: expected ‘=’, but found ‘:’
            error at 2487..2488: expected ‘=’, but found ‘:’
            error at 2489..2490: expected ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’, but found ‘[’
            error at 2490..2491: expected value-id, ‘self’, or ‘...’, but found number
            error at 2491..2492: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘,’
            error at 2503..2504: expected ‘=’, but found ‘]’
            error at 2514..2515: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, value-id, ‘self’, or ‘...’, but found ‘:’
            error at 2528..2529: expected ‘=’, but found ‘-’
            error at 2537: missing dedent
            error at 2572..2572: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
        "#]],
    );
}

#[test]
fn sample_lists_its_parse_errors() {
    check_errors(include_str!("../../examples/sample.kitty"), expect![""]);
}
