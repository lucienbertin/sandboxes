# Use a Rust base image with Cargo installed
FROM rust:1-bullseye AS builder

# Set the working directory inside the container
WORKDIR /usr/src/app

# Copy the Cargo.toml and Cargo.lock files
COPY Cargo.toml Cargo.lock ./
RUN mkdir domain
RUN mkdir shell
COPY domain/Cargo.toml Cargo.lock ./domain
COPY shell/Cargo.toml Cargo.lock ./shell

# Create an empty src directory to trick Cargo into thinking it's a valid Rust project
RUN mkdir domain/src && echo "fn main() {}" > domain/src/main.rs
RUN mkdir shell/src && echo "fn main() {}" > shell/src/main.rs

# Build the dependencies without the actual source code to cache dependencies separately
RUN cargo build --release --locked

# Now copy the source code
COPY ./domain/src ./domain/src
COPY ./shell/src ./shell/src

# Build your application
RUN cargo build --release --bin server --features api --no-default-features

# Start a new stage to create a smaller image without unnecessary build dependencies
FROM debian:bullseye-slim

# Set the working directory
WORKDIR /usr/src/app

# Copy the built binary from the previous stage
COPY --from=builder /usr/src/app/target/release/server ./

# Command to run the application
CMD ["./server"]