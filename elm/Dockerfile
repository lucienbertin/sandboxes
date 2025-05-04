FROM codesimple/elm:0.19 AS builder

WORKDIR /app

COPY elm.json ./
COPY src ./src

RUN elm make src/Main.elm --optimize --output dist/index.html
RUN elm make src/Posts.elm --optimize --output dist/posts/index.html
RUN elm make src/Health.elm --optimize --output dist/health/index.html

FROM busybox:1.35 AS runner

# Create a non-root user to own the files and run our server
RUN adduser -D static
USER static

WORKDIR /app

# Copy the static website
# Use the .dockerignore file to control what ends up inside the image!
COPY --from=builder app/dist .

# Run BusyBox httpd
CMD ["busybox", "httpd", "-f", "-v", "-p", "3000"]

