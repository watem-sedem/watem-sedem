FROM debian:bookworm
RUN apt update && apt -y install build-essential debhelper devscripts lazarus gpg git-buildpackage rsync apt-utils openssh-client git-lfs --no-install-recommends && apt clean
LABEL org.opencontainers.image.source https://github.com/watem-sedem/watem-sedem
