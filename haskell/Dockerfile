# Use a Haskell base image with cabal
FROM haskell:9.4.8-slim AS builder

# Set the working directory inside the container
WORKDIR /usr/src/app

RUN cabal update

# Copy the .cabal file
COPY haskell.cabal ./
# Create bogus source files
RUN mkdir lib && echo "module Schedule () where" > lib/Schedule.hs
RUN mkdir app && echo "main = putStrLn \"Hello World!\"" > app/Main.hs
RUN mkdir tests && echo "main = putStrLn \"Hello World!\"" > tests/ScheduleTest.hs

RUN cabal install --only-dependencies

# Now copy the source code
COPY ./app ./app
COPY ./lib ./lib

# Build code
RUN cabal build

# Start a new stage to create a smaller image without unnecessary build dependencies
FROM debian:bullseye-slim

# Set the working directory
WORKDIR /usr/src/app

# Copy the built binary from the previous stage
COPY --from=builder /usr/src/app/dist-newstyle/build/x86_64-linux/ghc-9.4.8/haskell-0.1.0.0/x/haskell/build/haskell ./

# For now just run with cabal
CMD ["./haskell"]
