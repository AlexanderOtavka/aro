init:
	cargo install cargo-watch

test:
	cargo watch -c -w src -x test
