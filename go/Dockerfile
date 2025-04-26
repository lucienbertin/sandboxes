FROM golang:1.24 AS builder

WORKDIR /app

COPY go.mod go.sum ./

RUN go mod download

COPY *.go ./

RUN CGO_ENABLED=0 GOOS=linux go build -o go-binary

FROM debian:bullseye-slim

WORKDIR /app

COPY --from=builder /app/go-binary ./

CMD ["./go-binary"]

