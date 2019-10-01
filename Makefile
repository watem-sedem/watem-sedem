SUBDIRS := $(wildcard */.)

all: CN_WS_console

CN_WS_console:
	$(MAKE) -C CN_WS_console

clean:
	$(MAKE) -C CN_WS_console clean

install: CN_WS_console
	install -D CN_WS_console/CN_WSmodel $(DESTDIR)$(prefix)/bin/CN_WSmodel

.PHONY: all CN_WS_console
