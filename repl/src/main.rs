use kitty_core::{lex, parse, ErrorReports, Solver, SourceId, Vm};
use std::{env, fs, path::PathBuf};

fn main() {
    let filename = env::args().nth(1).expect("Expected file argument");
    let filepath: PathBuf = filename.clone().into();
    let code = fs::read_to_string(&filepath).expect("Failed to read file");
    let source = SourceId::from_path(&filepath);

    let (tokens, lex_errors) = lex(&code, &source);

    if !lex_errors.is_empty() {
        ErrorReports::from(lex_errors).display(&code, source);
    }

    let Some(tokens) = tokens else {
        return;
    };

    let (expr, parse_errors) = parse(&code, &source, &tokens);

    if !parse_errors.is_empty() {
        ErrorReports::from(parse_errors).display(&code, source);
        return;
    }

    let Some(expr) = expr else {
        return;
    };

    let mut solver = Solver::default();
    let program_ty = solver
        .check(&expr, &mut Vec::new())
        .expect("Failed to check type");
    let solve_ty = solver.solve(program_ty).expect("Failed to solve type");
    println!("Result type: {:?}", solve_ty);

    let mut vm = Vm::default();
    println!("Result: {:?}", vm.eval(&expr));
}
