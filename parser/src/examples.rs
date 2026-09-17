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
            error at 278: missing ‘in’
            error at 278: missing ‘+’, ‘-’, ‘not’, value-id, ‘self’, type-id, ‘Self’, number, string, ‘(’, indent, ‘fn’, ‘let’, ‘if’, or ‘match’
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
            error at 1125..1129: expected ‘=’, but found ‘self’
            error at 1153..1163: expected dedent, but found value-id
            error at 1170..1181: expected ‘in’, but found value-id
            error at 1206..1225: expected dedent, but found value-id
            error at 1230..1232: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘in’
            error at 1237..1240: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘let’
            error at 1241..1258: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1259..1260: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 1261..1263: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘if’
            error at 1264..1283: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1284..1288: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘then’
            error at 1289..1300: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1301..1302: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
            error at 1303..1314: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1315..1319: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘else’
            error at 1320..1331: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1332..1334: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘in’
            error at 1339..1342: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘let’
            error at 1343..1361: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1362..1363: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 1364..1366: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘if’
            error at 1367..1386: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1387..1391: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘then’
            error at 1392..1393: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 1393..1394: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1395..1399: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘else’
            error at 1400..1401: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1402..1404: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘in’
            error at 1409..1412: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘let’
            error at 1413..1429: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1430..1431: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 1432..1434: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘if’
            error at 1435..1454: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1455..1459: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘then’
            error at 1460..1470: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1471..1472: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 1473..1474: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1475..1479: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘else’
            error at 1480..1490: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1491..1493: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘in’
            error at 1499..1504: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1509..1511: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1511..1520: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1520..1521: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 1521..1523: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1530..1532: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1532..1533: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1533..1534: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1535..1536: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 1536..1537: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1537..1538: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 1539..1549: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1549..1550: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 1559..1560: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1560..1561: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1562..1563: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 1563..1581: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1581..1582: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 1583..1599: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1599..1600: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 1609..1610: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1610..1611: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1612..1616: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1616..1622: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1630..1630: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 1630..1632: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘if’
            error at 1633..1652: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1659..1661: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1661..1670: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1670..1671: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 1671..1673: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1682..1684: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1684..1685: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1685..1686: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1687..1688: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 1688..1689: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1689..1690: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 1691..1701: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1701..1702: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 1713..1714: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1714..1715: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1716..1726: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1727..1728: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 1729..1730: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1741..1742: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1742..1743: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1744..1745: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 1745..1756: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1757..1758: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
            error at 1759..1760: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1760..1761: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 1762..1773: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1774..1775: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
            error at 1776..1777: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1778..1779: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘+’
            error at 1780..1791: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1791..1792: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 1803..1806: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1806..1807: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1808..1809: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 1809..1812: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1812..1813: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘error’
            error at 1821..1821: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 1821..1821: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 1821..1829: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1829..1830: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 1830..1831: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1838..1840: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1840..1841: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1841..1842: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1843..1844: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1853..1854: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1854..1855: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1856..1857: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1866..1867: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1867..1868: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1869..1870: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 1870..1871: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1871..1872: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 1873..1884: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1884..1885: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 1893..1893: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 1893..1901: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1901..1902: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 1902..1903: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1910..1912: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1912..1913: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1913..1914: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1915..1925: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1926..1927: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 1928..1929: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1938..1939: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1939..1940: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1941..1942: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1951..1952: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1952..1953: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 1954..1955: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 1955..1956: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 1956..1957: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 1958..1969: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1969..1970: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 1978..1978: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 1978..1986: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1986..1987: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 1987..1988: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 1995..1997: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 1997..1998: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 1998..1999: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2000..2001: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2010..2011: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2011..2012: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2013..2023: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2024..2025: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2026..2027: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2036..2037: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2037..2038: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2039..2040: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 2040..2041: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2041..2042: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 2043..2060: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2060..2061: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 2069..2069: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2069..2077: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2077..2078: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 2078..2079: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2086..2088: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 2088..2089: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2089..2090: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2091..2101: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2102..2103: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2104..2105: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2114..2115: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2115..2116: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2117..2127: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2128..2129: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2130..2131: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2140..2141: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2141..2142: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2143..2144: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 2144..2145: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2145..2146: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 2147..2164: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2164..2165: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 2173..2173: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2173..2181: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2181..2182: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 2182..2183: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2190..2192: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 2192..2193: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2193..2194: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2195..2196: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 2196..2197: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2197..2198: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 2199..2209: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2209..2210: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 2219..2220: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2220..2221: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2222..2223: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2232..2233: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2233..2234: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2235..2246: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2247..2248: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2249..2250: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2258..2258: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2258..2266: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2266..2267: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 2267..2268: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2275..2277: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 2277..2278: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2278..2279: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2280..2281: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 2281..2282: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2282..2283: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 2284..2294: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2294..2295: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 2304..2305: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2305..2306: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2307..2317: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2318..2319: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2320..2321: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2330..2331: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2331..2332: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2333..2344: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2345..2346: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2347..2348: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2356..2356: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2356..2364: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2364..2365: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 2365..2366: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2373..2375: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 2375..2376: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2376..2377: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2378..2379: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2388..2389: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2389..2390: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2391..2392: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 2392..2393: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2393..2394: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 2395..2405: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2405..2406: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 2415..2416: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2416..2417: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2418..2429: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2430..2431: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2432..2433: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2441..2441: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2441..2449: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2449..2450: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘.’
            error at 2450..2451: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found type-id
            error at 2458..2460: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found indent
            error at 2460..2461: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2461..2462: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2463..2473: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2474..2475: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2476..2477: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2486..2487: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2487..2488: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2489..2490: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘[’
            error at 2490..2491: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2491..2492: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 2493..2503: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2503..2504: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘]’
            error at 2513..2514: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2514..2515: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘:’
            error at 2516..2527: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 2528..2529: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘-’
            error at 2530..2531: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 2537..2537: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2537..2537: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2572..2572: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
            error at 2572..2572: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
        "#]],
    );
}

#[test]
fn sample_lists_its_parse_errors() {
    check_errors(
        include_str!("../../examples/sample.kitty"),
        expect![[r#"
            error at 53..56: expected ‘in’, but found ‘let’
            error at 61..62: expected dedent, but found ‘=’
            error at 66: missing value-id or ‘from’
            error at 84..87: expected ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, ‘(’, ‘{’, indent, ‘.’, ‘*’, ‘/’, ‘rem’, ‘+’, ‘-’, ‘<’, ‘<=’, ‘>’, ‘>=’, ‘==’, ‘!=’, ‘and’, ‘xor’, ‘or’, ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘let’
            error at 88..89: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 90..91: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘=’
            error at 92..95: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 95..96: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 96..99: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 99..100: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 100..101: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 101..102: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 103..105: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 105..106: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 106..107: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 108..109: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 109..110: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 113..116: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 116..117: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘(’
            error at 117..118: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found value-id
            error at 118..119: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘,’
            error at 120..123: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found number
            error at 123..124: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found ‘)’
            error at 125..125: expected ‘import’, ‘export’, ‘@’, ‘type’, ‘const’, ‘fn’, ‘enum’, ‘struct’, ‘trait’, or ‘impl’, but found dedent
        "#]],
    );
}
