# Citations:
#   Jonathan Sadun helped with the cargo-watch task

# Local Commands

build-release:
	cargo build --release
	cp target/release/aro .

~/.cargo/bin/cargo-watch:
	cargo install --root ~/.cargo cargo-watch

test: ~/.cargo/bin/cargo-watch
	cargo watch -w src -s "clear && printf '\e[3J' && cargo test"

clean:
	cargo clean
	rm -f ./aro

# Docker Commands

docker-build:
	docker build -t aro .

docker-run: docker-build
	docker run --rm aro cargo run -- $(args)

docker-test: docker-build
	docker run --rm aro cargo check --features "$(features)"
	docker run --rm aro cargo test --features "$(features)"
