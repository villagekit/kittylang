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

# build the playground's wasm into `playground/web/pkg/` (ignored by git)
# and say how to open the page. Adds the `wasm32-unknown-unknown` target
# if it is missing and installs `wasm-bindgen-cli` at the version
# `Cargo.lock` pins for the `wasm-bindgen` crate, since the two must
# match; a fresh machine's first run pays for both. The tool wants a
# newer Rust than the workspace's `rust-version` (1.86 for 0.2.126), and
# builds on the active toolchain, so the pin is not a bound on it. The
# page must be served over HTTP: a browser will not fetch the wasm from
# `file://`.
playground:
    #!/usr/bin/env sh
    set -eu
    rustup target list --installed | grep -qx wasm32-unknown-unknown || rustup target add wasm32-unknown-unknown
    version="$(cargo pkgid wasm-bindgen)"; version="${version##*@}"
    installed="$(wasm-bindgen --version 2>/dev/null | cut -d' ' -f2 || true)"
    if [ "$installed" != "$version" ]; then
        cargo install wasm-bindgen-cli --version "$version" --locked
    fi
    cargo build -p kitty-playground --target wasm32-unknown-unknown --release
    wasm-bindgen --target web --out-dir playground/web/pkg target/wasm32-unknown-unknown/release/kitty_playground.wasm
    echo
    echo "Built playground/web/pkg/. Serve playground/web over HTTP and open it, for example:"
    echo "  python3 -m http.server -d playground/web 8000"
    echo "  then open http://localhost:8000/"
