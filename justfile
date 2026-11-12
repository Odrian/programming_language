set shell := ["bash", "-euo", "pipefail", "-c"]

ci:
    cargo fmt --all
    cargo check --workspace --all-targets --quiet
    cargo clippy --workspace --all-targets --quiet
    cargo build --workspace --quiet
    cargo test --workspace --all-targets --quiet --no-fail-fast

stats:
    nix run nixpkgs#cloc -- . --include-ext=rs --by-file --not-match-f='_test\.rs$'
