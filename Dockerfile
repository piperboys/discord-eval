# Use the official Haskell image with stack
FROM haskell:9.6.5

# Set the working directory inside the container
WORKDIR /app

# Copy stack configuration and project files
COPY stack.yaml stack.yaml
COPY stack.yaml.lock stack.yaml.lock
COPY package.yaml package.yaml

# Install system dependencies
RUN apt-get update && \
    apt-get install -y libgmp-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Run hpack to generate the .cabal file from package.yaml
RUN stack setup --resolver lts-22.26
RUN stack build --only-dependencies --system-ghc

# Copy the rest of the project files
COPY . .
COPY piper/piper /usr/bin/piper

# Build the project
RUN stack build --system-ghc

# Expose the port that the application will run on (if applicable)
# EXPOSE 8080

# Command to run the application
CMD ["stack", "run"]

