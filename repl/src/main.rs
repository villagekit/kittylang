use kitty_core::{lexer, parser, Solver, Span, Vm};
use std::{env, fmt, fs};

fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");
    let src = &fs::read_to_string(&filename).expect("Failed to read file");

    let tokens = lexer()
        .parse(src)
        .into_result()
        .unwrap_or_else(|errs| parse_failure(&errs[0], src));

    let expr = parser(make_input)
        .parse(make_input((0..src.len()).into(), &tokens))
        .into_result()
        .unwrap_or_else(|errs| parse_failure(&errs[0], src));

    let mut solver = Solver {
        src,
        vars: Vec::new(),
    };
    let program_ty = solver.check(&expr, &mut Vec::new());
    println!("Result type: {:?}", solver.solve(program_ty));

    let mut vm = Vm::default();
    println!("Result: {:?}", vm.eval(&expr));
}

fn make_input<'src>(
    eoi: SimpleSpan,
    toks: &'src [Spanned<Token<'src>>],
) -> impl BorrowInput<'src, Token = Token<'src>, Span = SimpleSpan> {
    toks.map(eoi, |(t, s)| (t, s))
}
