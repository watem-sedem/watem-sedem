docker build . -t registry.fluves.net/drone/cn_ws/debian11-build
docker build . -f Dockerfile-cnws_test -t registry.fluves.net/cnws_test
