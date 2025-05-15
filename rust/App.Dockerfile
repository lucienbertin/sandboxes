# # https://book.leptos.dev/deployment/ssr.html#debian
# # Use a Rust base image with Cargo installed
# FROM rust:1-alpine3.21 AS builder

# RUN apk update && \
#     apk add --no-cache bash binaryen gcc git g++ libc-dev make npm openssl-dev protobuf-dev protoc

# RUN rustup target add wasm32-unknown-unknown
# RUN rustup component add clippy

# RUN wget https://github.com/cargo-bins/cargo-binstall/releases/latest/download/cargo-binstall-x86_64-unknown-linux-musl.tgz
# RUN tar -xvf cargo-binstall-x86_64-unknown-linux-musl.tgz
# RUN cp cargo-binstall /usr/local/cargo/bin

# # Install required tools
# # RUN apt-get update -y \
# #   && apt-get install -y --no-install-recommends clang
# RUN apk update && \
#     apk add --no-cache bash curl npm libc-dev binaryen perl openssl

# RUN npm install -g sass

# RUN curl --proto '=https' --tlsv1.3 -LsSf https://github.com/leptos-rs/cargo-leptos/releases/latest/download/cargo-leptos-installer.sh | sh

# # Install cargo-leptos
# RUN cargo binstall cargo-leptos -y
# # RUN cargo install cargo-generate
# # RUN cargo install cargo-leptos
# # RUN curl --proto '=https' --tlsv1.3 -LsSf https://github.com/leptos-rs/cargo-leptos/releases/latest/download/cargo-leptos-installer.sh | sh
# # RUN npm install -g sass

# # Set the working directory inside the container
# WORKDIR /usr/src/app

# # Copy the Cargo.toml and Cargo.lock files
# COPY Cargo.toml Cargo.lock ./
# RUN mkdir domain
# RUN mkdir shell
# COPY domain/Cargo.toml ./domain
# COPY shell/Cargo.toml ./shell

# # Create an empty src directory to trick Cargo into thinking it's a valid Rust project
# RUN mkdir domain/src && echo "fn main() {}" > domain/src/main.rs
# RUN mkdir shell/src && echo "fn main() {}" > shell/src/main.rs
# RUN echo "" > shell/src/lib.rs
# # RUN echo "fn main() {}" > shell/src/main-api.rs
# RUN echo "fn main() {}" > shell/src/main-app.rs
# # RUN echo "fn main() {}" > shell/src/main-worker.rs

# # Build the dependencies without the actual source code to cache dependencies separately
# RUN cargo build --release --locked --features="appssr,appcsr" --no-default-features

# # Now copy the source code
# COPY ./domain/src ./domain/src
# COPY ./shell/src ./shell/src

# # Build your application
# RUN cargo leptos build --release -vv
# # RUN cargo build --release --package=shell --lib --target-dir=/home/lulu/Documents/code/sandboxes/rust/target/front --target=wasm32-unknown-unknown --features=appcsr
# # RUN cargo build --release --package=shell --bin=shell --features=appssr

# # Start a new stage to create a smaller image without unnecessary build dependencies
# FROM debian:bullseye-slim

# # Set the working directory
# WORKDIR /usr/src/app

# # Copy the built binary from the previous stage
# COPY --from=builder /app/target/release/shell /app/

# # /target/site contains our JS/WASM/CSS, etc.
# COPY --from=builder /app/target/site /app/site

# # Command to run the application
# CMD ["./shell"]

# Get started with a build env with Rust nightly
FROM rust:1-alpine3.21 AS builder

RUN apk update && \
    apk add --no-cache bash binaryen gcc git g++ libc-dev make npm openssl-dev protobuf-dev protoc perl libpq-dev

RUN rustup target add wasm32-unknown-unknown
RUN rustup component add clippy

# RUN cargo install cargo-generate
RUN wget https://github.com/cargo-bins/cargo-binstall/releases/latest/download/cargo-binstall-x86_64-unknown-linux-musl.tgz
RUN tar -xvf cargo-binstall-x86_64-unknown-linux-musl.tgz
RUN cp cargo-binstall /usr/local/cargo/bin
RUN cargo binstall cargo-leptos -y
# RUN cargo install cargo-leptos --locked

RUN npm install -g sass

WORKDIR /work
# Copy the Cargo.toml and Cargo.lock files
COPY Cargo.toml Cargo.lock ./
RUN mkdir domain
RUN mkdir shell
COPY domain/Cargo.toml ./domain
COPY shell/Cargo.toml ./shell

# Create an empty src directory to trick Cargo into thinking it's a valid Rust project
RUN mkdir domain/src && echo "fn main() {}" > domain/src/main.rs
RUN mkdir shell/src && echo "fn main() {}" > shell/src/main.rs
RUN echo "" > shell/src/lib.rs
# RUN echo "fn main() {}" > shell/src/main-api.rs
RUN echo "fn main() {}" > shell/src/main-app.rs
# RUN echo "fn main() {}" > shell/src/main-worker.rs
# RUN cargo install cargo-leptos --locked

# # Build the dependencies without the actual source code to cache dependencies separately
RUN cargo build --release --locked --features="appssr,appcsr" --no-default-features

# copy source code and build
COPY ./domain/src ./domain/src
COPY ./shell/src ./shell/src
COPY ./shell/public ./shell/public
COPY ./shell/style ./shell/style
RUN cargo leptos build --release -vv

FROM rust:1-alpine3.21 AS runner

WORKDIR /app

COPY --from=builder /work/target/release/shell /app/
COPY --from=builder /work/target/site /app/site
# COPY --from=builder /work/Cargo.toml /app/

# ENV RUST_LOG="info"
# ENV LEPTOS_SITE_ADDR="0.0.0.0:8080"
# ENV LEPTOS_SITE_ROOT=./site
# EXPOSE 8080

CMD ["/app/shell"]