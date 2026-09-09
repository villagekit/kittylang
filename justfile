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
