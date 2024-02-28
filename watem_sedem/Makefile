watem_sedem:
	echo \'$(shell git describe --abbrev=4 --always --tags)\' > version.inc
	lazbuild watem_sedem.lpr
	echo 'DEVELOP' > version.inc
all: watem_sedem

clean:
	rm -f watem_sedem
