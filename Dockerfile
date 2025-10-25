FROM ubuntu:latest

# Update the system, install OpenSSH Server, curl, and adduser
RUN apt-get update -y && \
    apt-get install -y openssh-server curl adduser sudo xz-utils

# Remove ubuntu user if exists and create nyc user with UID 1000
RUN userdel -r ubuntu 2>/dev/null || true && \
    adduser --disabled-password --gecos "" --uid 1000 nyc

# Add the 'nyc' user to the sudo group for passwordless sudo access
RUN echo "nyc ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

# Switch to nyc user to install Nix
USER nyc
WORKDIR /home/nyc

# Install Nix (Determinate Systems installer) as nyc user
RUN curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install linux \
  --extra-conf "sandbox = false" \
  --init none \
  --no-confirm

# Switch back to root to fix permissions
USER root

# Fix Nix permissions so nyc user can use it
RUN chown -R nyc:nyc /nix && \
    chmod -R 755 /nix

# Switch back to nyc user
USER nyc

# Add Nix to the PATH and set up environment
ENV PATH="/nix/var/nix/profiles/default/bin:${PATH}"

# Set up rstats-on-nix cache for faster package downloads
RUN mkdir -p /home/nyc/.config/nix && \
    echo "substituters = https://cache.nixos.org https://rstats-on-nix.cachix.org" > /home/nyc/.config/nix/nix.conf && \
    echo "trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= rstats-on-nix.cachix.org-1:vdiiVgocg6WeJrODIqdprZRUrhi1JzhBnXv7aWI6+F0=" >> /home/nyc/.config/nix/nix.conf

# Copy default.nix (assumes it's in the build context)
COPY --chown=nyc:nyc default.nix /home/nyc/

# Build the Nix environment as nyc user
RUN nix-build /home/nyc/default.nix

# Install direnv and nix-direnv for Positron integration
RUN nix-env -f '<nixpkgs>' -iA direnv nix-direnv

# Ensure Nix profile is available in bashrc
RUN echo 'if [ -e /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh ]; then . /nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh; fi' >> /home/nyc/.bashrc

# Set up .ssh directory with proper permissions
RUN mkdir -p /home/nyc/.ssh && \
    chmod 700 /home/nyc/.ssh && \
    touch /home/nyc/.ssh/authorized_keys && \
    chmod 600 /home/nyc/.ssh/authorized_keys

# Switch back to root for SSH setup
USER root

# Set up SSH configuration
RUN mkdir -p /var/run/sshd && \
    echo "PasswordAuthentication no" >> /etc/ssh/sshd_config && \
    echo "PermitRootLogin no" >> /etc/ssh/sshd_config && \
    echo "PubkeyAuthentication yes" >> /etc/ssh/sshd_config && \
    echo "AllowUsers nyc" >> /etc/ssh/sshd_config && \
    echo "LogLevel DEBUG2" >> /etc/ssh/sshd_config && \
    echo "PubkeyAcceptedKeyTypes +ssh-rsa" >> /etc/ssh/sshd_config

# Expose SSH port
EXPOSE 22

# Run SSH daemon as root
CMD ["/usr/sbin/sshd", "-D"]
