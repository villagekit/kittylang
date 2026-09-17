//! The `kitty` command prints what the compiler sees. `kitty lex <file>`
//! prints the token stream, one token per line. `kitty parse <file>`
//! prints the syntax tree, then each parse error as a report with the
//! source line and a label. The exit code is 1 when the file has errors,
//! 2 when the file cannot be read or the output cannot be written.
//!
//! This is the pipeline's first I/O boundary. It moves text between the
//! file, the library crates and stdout, and decides nothing itself.

use std::{
    fs,
    io::{self, BufWriter, Write},
    path::{Path, PathBuf},
    process::ExitCode,
};

use clap::{Parser, Subcommand};

use kitty_lexer::{lex, TokenKind, Tokens};
use kitty_meta::{render, SourceId};

#[derive(Parser)]
#[command(name = "kitty", version, about = "Prints what the compiler sees")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Print the token stream, one token per line
    Lex {
        /// The `.kitty` file to lex
        file: PathBuf,
    },
    /// Print the syntax tree, then each parse error as a report
    Parse {
        /// The `.kitty` file to parse
        file: PathBuf,
    },
}

/// What stops the command before it can print a result.
#[derive(Debug, thiserror::Error)]
enum CliError {
    /// The file does not exist or cannot be opened. Check the path and
    /// the permissions.
    #[error("reading {path}: {error}")]
    Read { path: PathBuf, error: io::Error },

    /// The file's bytes are not UTF-8. Kitty source is UTF-8 text; convert
    /// the file.
    #[error("reading {path}: not UTF-8")]
    NotUtf8 { path: PathBuf },

    /// Writing the output failed. Nothing to fix in the source. A closed
    /// pipe is not reported, since nothing is left to read the report.
    #[error("writing output: {0}")]
    Write(io::Error),
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    let stdout = io::stdout();
    let mut out = BufWriter::new(stdout.lock());
    match run(&cli.command, &mut out).and_then(|has_errors| {
        out.flush().map_err(CliError::Write)?;
        Ok(has_errors)
    }) {
        Ok(false) => ExitCode::SUCCESS,
        Ok(true) => ExitCode::from(1),
        // The reader closed the pipe (`kitty parse f | head`): nothing to tell it.
        Err(CliError::Write(error)) if error.kind() == io::ErrorKind::BrokenPipe => {
            ExitCode::from(2)
        }
        Err(error) => {
            eprintln!("error: {error}");
            ExitCode::from(2)
        }
    }
}

/// Runs one subcommand, writing its output to `out`. Returns whether the
/// source had errors.
fn run(command: &Command, out: &mut impl Write) -> Result<bool, CliError> {
    match command {
        Command::Lex { file } => {
            let text = read_source(file)?;
            let tokens: Tokens = lex(&text).into();
            write!(out, "{tokens}").map_err(CliError::Write)?;
            let has_errors = tokens.iter().any(|token| token.kind == TokenKind::Error);
            Ok(has_errors)
        }
        Command::Parse { file } => {
            let text = read_source(file)?;
            let parse = kitty_parser::parse(&text);
            write!(out, "{:#?}", parse.tree).map_err(CliError::Write)?;
            let source = SourceId::from_path(file);
            for error in &parse.errors {
                let report = render(
                    source,
                    &text,
                    "syntax error",
                    &[(error.range(), error.message())],
                );
                write!(out, "\n{report}").map_err(CliError::Write)?;
            }
            Ok(!parse.errors.is_empty())
        }
    }
}

fn read_source(path: &Path) -> Result<String, CliError> {
    let bytes = fs::read(path).map_err(|error| CliError::Read {
        path: path.to_path_buf(),
        error,
    })?;
    String::from_utf8(bytes).map_err(|_| CliError::NotUtf8 {
        path: path.to_path_buf(),
    })
}
