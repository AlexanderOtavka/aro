build:
	cargo build

install:
	cargo install cargo-watch

test:
	cargo watch -c -w src -x check -x test

test-once:
	cargo check
	cargo test
