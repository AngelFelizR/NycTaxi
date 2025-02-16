FROM ghcr.io/rocker-org/geospatial:4.4.2

# Install Node.js and npm
RUN apt-get update && apt-get install -y curl

# Download the Node.js setup script (remove GPG verification)
RUN curl --location --fail --silent https://deb.nodesource.com/setup_18.x --output nodesource_setup.sh && \
    bash nodesource_setup.sh && \
    rm nodesource_setup.sh || exit 1

RUN apt-get install -y nodejs

# Verify Node.js installation
RUN node -v && npm -v

# Set the PATH (removing any duplication)
RUN export PATH="/usr/bin:/usr/local/bin:$PATH"
RUN PATH=$(echo $PATH | tr ':' '\n' | sort -u | tr '\n' ':')

ENV PATH=$PATH
