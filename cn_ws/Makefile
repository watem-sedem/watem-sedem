cn_ws:
	echo \'$(shell git describe --abbrev=4 --dirty --always --tags)\' > version.inc
	lazbuild cn_ws.lpr
	rm version.inc

all: cn_ws

clean:
	rm -f cn_ws
