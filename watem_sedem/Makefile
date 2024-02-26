cn_ws:
	echo \'$(shell git describe --abbrev=4 --always --tags)\' > version.inc
	lazbuild cn_ws.lpr
	echo 'DEVELOP' > version.inc
all: cn_ws

clean:
	rm -f cn_ws
