docker build . -t registry.fluves.net/drone/watem-sedem/debian11-build -t ghcr.io/watem-sedem/ci/debian11-build
docker build . -f Dockerfile-test -t registry.fluves.net/drone/watem-sedem/watem-sedem_test -t ghcr.io/watem-sedem/ci/watem-sedem-test
