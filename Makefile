# Citations:
#   Jonathan Sadun helped with the cargo-watch task

# Docker Commands

docker-build:
	docker build -t aro .

docker-run: docker-build
	docker run --rm aro

docker-test: docker-build
	docker run --rm aro cargo test

# Local Commands

~/.cargo/bin/cargo-watch:
	cargo install cargo-watch

test: ~/.cargo/bin/cargo-watch
	cargo watch -c -w src -x check -x test
