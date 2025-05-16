# Use a Rust base image with Cargo installed
FROM rust:1-bookworm AS builder

# Install cargo-binstall, which makes it easier to install other
# cargo extensions like cargo-leptos
RUN wget https://github.com/cargo-bins/cargo-binstall/releases/latest/download/cargo-binstall-x86_64-unknown-linux-musl.tgz
RUN tar -xvf cargo-binstall-x86_64-unknown-linux-musl.tgz
RUN cp cargo-binstall /usr/local/cargo/bin

# Install required tools
RUN apt-get update -y \
  && apt-get install -y --no-install-recommends clang

# Install cargo-leptos
RUN cargo binstall cargo-leptos -y

# Add the WASM target
RUN rustup target add wasm32-unknown-unknown

# Set the working directory inside the container
WORKDIR /usr/src/app

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

# Build the dependencies without the actual source code to cache dependencies separately
RUN cargo build --release --locked --features="appssr,appcsr" --no-default-features

# Now copy the source code
COPY ./domain/src ./domain/src
COPY ./shell/src ./shell/src
COPY ./shell/public ./shell/public
COPY ./shell/style ./shell/style

# Build your application
RUN cargo leptos build --release

# Start a new stage to create a smaller image without unnecessary build dependencies
FROM debian:bookworm-slim

# Set the working directory
WORKDIR /usr/src/app

# Copy the built binary from the previous stage
COPY --from=builder /usr/src/app/target/release/shell ./
COPY --from=builder /usr/src/app/target/site ./site

# Command to run the application
CMD ["./shell"]