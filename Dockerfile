FROM ubuntu:24.04

# ── Layer 1: apt base ────────────────────────────────────────────────────────
RUN apt update -y && \
    apt install -y locales curl openssh-server xz-utils && \
    locale-gen en_US.UTF-8 && \
    update-locale LANG=en_US.UTF-8 && \
    rm -rf /var/lib/apt/lists/*
 
ENV LANG=en_US.UTF-8 \
    LANGUAGE=en_US:en \
    LC_ALL=en_US.UTF-8

# ── Layer 2: Nix Installation (Docker Optimized) ────────────────────────────
RUN curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | \
    sh -s -- install linux \
    --init none \
    --no-confirm

RUN mkdir -p /etc/nix && echo "sandbox = false" >> /etc/nix/nix.conf

# BASH_ENV ensures all subsequent RUN layers automatically load Nix!
ENV PATH="${PATH}:/root/.nix-profile/bin:/nix/var/nix/profiles/default/bin" \
    BASH_ENV=/nix/var/nix/profiles/default/etc/profile.d/nix.sh \
    user=root

# ── Layer 3: direnv / nix-direnv (Positron integration) ─────────────────────
RUN nix-env -f '<nixpkgs>' -iA direnv nix-direnv && \
    nix-collect-garbage -d

# Fixed the .bashrc typo here (removed duplicate 'c')
RUN echo '. /nix/var/nix/profiles/default/etc/profile.d/nix.sh' >> /root/.bashrc

# ── Layer 4: fetch and cache the nixpkgs tarball ─────────────────────────────
COPY nix/pkgs.nix /root/nix/pkgs.nix
RUN nix-instantiate --eval /root/nix/pkgs.nix && \
    nix-collect-garbage -d

# ── Layer 5: system packages (R, quarto, pandoc, fonts…) ────────────────────
COPY nix/system.nix /root/nix/system.nix
RUN nix-build /root/nix/system.nix -o /nix/profiles/system-packages && \
    nix-collect-garbage -d

# ── Layer 6: core R / tidyverse / tidymodels ─────────────────────────────────
COPY nix/r-core.nix /root/nix/r-core.nix
RUN nix-build /root/nix/r-core.nix -o /nix/profiles/r-core && \
    nix-collect-garbage -d

# ── Layer 7: ML / modelling packages ─────────────────────────────────────────
COPY nix/r-ml.nix /root/nix/r-ml.nix
RUN nix-build /root/nix/r-ml.nix -o /nix/profiles/r-ml && \
    nix-collect-garbage -d

# ── Layer 8: geo / spatial packages ──────────────────────────────────────────
COPY nix/r-geo.nix /root/nix/r-geo.nix
RUN nix-build /root/nix/r-geo.nix -o /nix/profiles/r-geo && \
    nix-collect-garbage -d

# ── Layer 9: data / IO packages ──────────────────────────────────────────────
COPY nix/r-data.nix /root/nix/r-data.nix
RUN nix-build /root/nix/r-data.nix -o /nix/profiles/r-data && \
    nix-collect-garbage -d

# ── Layer 10: custom-built packages (corrcat, pins, roxygen2) ────────────────
COPY nix/r-custom.nix /root/nix/r-custom.nix
RUN nix-build /root/nix/r-custom.nix -o /nix/profiles/r-custom && \
    nix-collect-garbage -d

# ── Layer 11: shell.nix for Positron / direnv ────────────────────────────────
COPY nix/shell.nix /root/nix/shell.nix
# Added comment to force clear cache for this specific layer
RUN nix-build /root/nix/shell.nix -o /nix/profiles/shell && \
    nix-collect-garbage -d

# ── SSH setup ────────────────────────────────────────────────────────────────
EXPOSE 22

RUN mkdir -p /var/run/sshd /root/.ssh && \
    chmod 700 /root/.ssh && \
    echo "PermitRootLogin prohibit-password" >> /etc/ssh/sshd_config && \
    echo "PubkeyAuthentication yes" >> /etc/ssh/sshd_config && \
    echo "AuthorizedKeysFile .ssh/authorized_keys" >> /etc/ssh/sshd_config

CMD ["/usr/sbin/sshd", "-D"]
