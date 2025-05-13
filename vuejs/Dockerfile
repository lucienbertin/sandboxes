FROM node:22-bullseye-slim AS base

# # Check https://github.com/nodejs/docker-node/tree/b4117f9333da4138b03a546ec926ef50a31506c3#nodealpine to understand why libc6-compat might be needed.
# RUN apk add --no-cache libc6-compat
FROM base AS builder
WORKDIR /app

# Install dependencies based on the preferred package manager
COPY package.json package-lock.json ./
RUN npm ci

# Rebuild the source code only when needed
COPY . .

RUN npm run build

# Production image, copy all the files and serve them
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
