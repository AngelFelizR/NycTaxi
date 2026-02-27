FROM ubuntu:24.04

# Use docker compose build --build-arg CACHE_BUST=$(date +%s)
# To for to update the image

ARG CACHE_BUST=1
# Update and install ALL packages in one layer, including locales
RUN apt update -y && \
    apt install -y locales curl openssh-server xz-utils && \
    locale-gen en_US.UTF-8 && \
    update-locale LANG=en_US.UTF-8

# Defining ports to share SSH
EXPOSE 22

# Defining SSH configuration
RUN mkdir -p /var/run/sshd && \
    mkdir -p /root/.ssh && \
    chmod 700 /root/.ssh && \
    echo "PermitRootLogin yes" >> /etc/ssh/sshd_config && \
    echo "PasswordAuthentication yes" >> /etc/ssh/sshd_config && \
    echo "PubkeyAuthentication yes" >> /etc/ssh/sshd_config && \
    echo "AuthorizedKeysFile .ssh/authorized_keys" >> /etc/ssh/sshd_config

RUN echo 'root:sbs' | chpasswd

# Set locale environment variables
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# The next line install Nix inside Docker
RUN bash -c 'sh <(curl --proto "=https" --tlsv1.2 -L https://nixos.org/nix/install) --daemon'

# Adds Nix to the path
ENV PATH="${PATH}:/nix/var/nix/profiles/default/bin"
ENV user=root

# Install direnv and nix-direnv for Positron integration
RUN nix-env -f '<nixpkgs>' -iA direnv nix-direnv

# Starting nix-shell
RUN echo '. /nix/var/nix/profiles/default/etc/profile.d/nix.sh' >> /root/.bashrc

# Defining enviroment with Nix
COPY default.nix .

# We now build the environment
RUN nix-build

# Start SSH server
CMD ["/usr/sbin/sshd", "-D"]
