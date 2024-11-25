docker build . -t ghcr.io/watem-sedem/ci/debian11-build
docker build . -f Dockerfile-test -t ghcr.io/watem-sedem/ci/watem-sedem-test
