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

# Production image, copy all the files and run next
FROM base AS runner
WORKDIR /app

COPY --from=builder /app/public ./public
COPY --from=builder /app/dist ./dist
COPY --from=builder /app/package.json ./
# COPY --from=builder /app/node_modules ./node_modules

CMD ["npm", "run", "serve:ssr:angular"]