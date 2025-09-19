FROM ubuntu:latest

# Update the system, install OpenSSH Server, and curl
RUN apt-get update -y && \
    apt-get install -y openssh-server && \
    apt-get install -y curl

# The next 4 lines install Nix inside Docker. See the Determinate Systems installer's documentation
RUN curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install linux \
  --extra-conf "sandbox = false" \
  --init none \
  --no-confirm

# Adds Nix to the path, as described by the Determinate Systems installer's documentation
ENV PATH="${PATH}:/nix/var/nix/profiles/default/bin"
ENV user=root

# Set up rstats-on-nix cache
# Thanks to the rstats-on-nix cache, precompiled binary packages will
# be downloaded instead of being compiled from source
RUN mkdir -p /root/.config/nix && \
    echo "substituters = https://cache.nixos.org https://rstats-on-nix.cachix.org" > /root/.config/nix/nix.conf && \
    echo "trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= rstats-on-nix.cachix.org-1:vdiiVgocg6WeJrODIqdprZRUrhi1JzhBnXv7aWI6+F0=" >> /root/.config/nix/nix.conf

# This will overwrite the default.nix
COPY default.nix .

# We now build the environment
RUN nix-build

# Crear usuario y configurar contraseñas
RUN echo "root:r" | chpasswd

# Installing direnv
RUN nix-env -f '<nixpkgs>' -iA direnv && \
    nix-env -f '<nixpkgs>' -iA nix-direnv

# Configuración de SSH
RUN mkdir /var/run/sshd && \
    echo "PermitRootLogin yes" >> /etc/ssh/sshd_config && \
    echo "PasswordAuthentication yes" >> /etc/ssh/sshd_config && \
    sed -i 's@session\s*required\s*pam_loginuid.so@session optional pam_loginuid.so@g' /etc/pam.d/sshd && \
    echo "export VISIBLE=now" >> /etc/profile

# Exponer puerto SSH
EXPOSE 22

# Run SSH
CMD ["/usr/sbin/sshd", "-D"]