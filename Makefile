# Citations:
#   Jonathan Sadun helped with the cargo-watch task

# Local Commands

build-release:
	cargo build --release
	cp target/release/aro .

~/.cargo/bin/cargo-watch:
	cargo install --root ~/.cargo cargo-watch

test: ~/.cargo/bin/cargo-watch
	cargo watch -c -w src -x test

clean:
	cargo clean

# Docker Commands

docker-build:
	docker build -t aro .

docker-run: docker-build
	docker run --rm aro cargo run -- $(args)

docker-test: docker-build
	docker run --rm aro cargo test --features "$(features)"
