# Use official Haskell image with stack
FROM haskell:9

# Install system dependencies
RUN apt-get update && apt-get install -y libpq-dev

# Set workdir
WORKDIR /app

# Copy stack files and install dependencies first (for better caching)
COPY stack.yaml  haskell-crm.cabal /app/
RUN stack setup && stack build --only-dependencies

# Copy the rest of the source
COPY . /app

# Build the executable
RUN stack build --copy-bins

# Expose the port
EXPOSE 8080

# Run the app
CMD ["/app/.local/bin/haskell-crm"]
