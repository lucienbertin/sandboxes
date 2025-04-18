# Use a Haskell base image with cabal
FROM haskell:9.4.8-slim AS builder

# Set the working directory inside the container
WORKDIR /usr/src/app

RUN cabal update

# Copy the .cabal file
COPY haskell.cabal ./
RUN mkdir app && echo "main = putStrLn \"Hello World!\"" > app/Main.hs
RUN mkdir tests && echo "main = putStrLn \"Hello World!\"" > tests/ScheduleTest.hs

RUN cabal install schedule-lib --only-dependencies --only-download
RUN cabal install haskell --only-dependencies --only-download

# Now copy the source code
COPY ./app ./app
COPY ./lib ./lib

RUN cabal build

# For now just run with cabal
CMD ["cabal", "run"]
