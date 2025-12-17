FROM ubuntu:latest

# Updating the system
RUN apt update -y

# Installing packages to use, including 'xz-utils' for the Nix installer
RUN apt install curl openssh-server xz-utils -y

# The next line install Nix inside Docker
# FIX: Using bash -c to enable process substitution (<()) and running the Nix installer.
RUN bash -c 'sh <(curl --proto "=https" --tlsv1.2 -L https://nixos.org/nix/install) --daemon'

# Adds Nix to the path, as described by the Determinate Systems installer's documentation
ENV PATH="${PATH}:/nix/var/nix/profiles/default/bin"
ENV user=root

# Set up rstats-on-nix cache
# Thanks to the rstats-on-nix cache, precompiled binary packages will
# be downloaded instead of being compiled from source
RUN mkdir -p /root/.config/nix && \
    echo "substituters = https://cache.nixos.org https://rstats-on-nix.cachix.org" > /root/.config/nix/nix.conf && \
    echo "trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= rstats-on-nix.cachix.org-1:vdiiVgocg6WeJrODIqdprZRUrhi1JzhBnXv7aWI6+F0=" >> /root/.config/nix/nix.conf

# Install direnv and nix-direnv for Positron integration
RUN nix-env -f '<nixpkgs>' -iA direnv nix-direnv

# Defining enviroment with Nix
COPY default.nix .

# We now build the environment
RUN nix-build

# Defining password for SSH 
RUN mkdir -p /var/run/sshd && \
    echo "PermitRootLogin yes" >> /etc/ssh/sshd_config && \
    echo "PasswordAuthentication yes" >> /etc/ssh/sshd_config
RUN echo 'root:sbs' | chpasswd

# Expose SSH port
EXPOSE 22

# Start SSH server
CMD ["/usr/sbin/sshd", "-D"]
