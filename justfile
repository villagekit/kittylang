# The local quality gate (DESIGN.md#engineering-substrate): no CI host before
# adoption; the operator and agents run this. Bound it: `timeout 600 just check`.

# fmt check, clippy with warnings as errors, tests
check: fmt-check clippy test

# tests only (the faster inner loop)
test:
    cargo test --workspace

# build the full workspace
build:
    cargo build --workspace

# clippy with warnings as errors
clippy:
    cargo clippy --workspace --all-targets -- -D warnings

# format all crates
fmt:
    cargo fmt --all

# formatting as a check, no writes
fmt-check:
    cargo fmt --all --check

# fuzz `kitty_parser::parse` for `seconds` (default 60), the corpus seeded
# from `examples/`. Needs the nightly toolchain (`rustup toolchain install
# nightly`) and installs `cargo-fuzz` if absent, which the first run pays
# for. Not part of `just check`: run on demand, bounded once installed:
# `timeout 120 just fuzz`. A finding lands in `fuzz/artifacts/parse/`; see
# `fuzz/README.md`.
fuzz seconds="60":
    command -v cargo-fuzz >/dev/null || cargo install cargo-fuzz --locked
    mkdir -p fuzz/corpus/parse
    cp examples/*.kitty fuzz/corpus/parse/
    cargo +nightly fuzz run parse -- -max_total_time={{seconds}} -timeout=10
