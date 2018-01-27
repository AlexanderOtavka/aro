FROM rust:1.23.0

WORKDIR /var/aro
COPY . .

CMD ["cargo run"]
