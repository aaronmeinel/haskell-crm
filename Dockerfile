
# Stage 1: Build
FROM debian:bullseye as build

# Install system dependencies
RUN apt-get update && apt-get install -y curl libpq-dev build-essential

# Install Stack
RUN curl -sSL https://get.haskellstack.org/ | sh

WORKDIR /app

# Copy stack files and install dependencies first (for better caching)
COPY stack.yaml haskell-crm.cabal package.yaml* /app/
RUN stack setup && stack build --only-dependencies

# Copy the rest of the source
COPY . /app

# Build the executable
RUN stack build --copy-bins

# Stage 2: Minimal runtime image
FROM debian:bullseye-slim

RUN apt-get update && apt-get install -y libpq5 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /app/.local/bin/haskell-crm /app/haskell-crm

EXPOSE 8080

CMD ["/app/haskell-crm"]
