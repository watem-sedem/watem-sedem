FROM debian:buster-slim
RUN apt update && apt -y install build-essential debhelper devscripts lazarus gpg git-buildpackage --no-install-recommends && apt clean
RUN apt update && apt -y install rsync apt-utils openssh-client --no-install-recommends && apt clean
