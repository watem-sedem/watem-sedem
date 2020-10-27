SUBDIRS := $(wildcard */.)

all: cn_ws

cn_ws:
	$(MAKE) -C cn_ws

clean:
	$(MAKE) -C cn_ws clean

install: cn_ws
	install -D cn_ws/cn_ws $(DESTDIR)$(prefix)/bin/cn_ws

.PHONY: all cn_ws
