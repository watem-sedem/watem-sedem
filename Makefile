SUBDIRS := $(wildcard */.)

all: tests cn_ws

cn_ws:
	$(MAKE) -C cn_ws

tests:
	$(MAKE) -C cn_ws
	$(MAKE) -C tests

integration_test:
	$(MAKE) -C cn_ws
	$(MAKE) -C tests
	testfiles/test.sh
	pytest testfiles

clean:
	$(MAKE) -C cn_ws clean
	$(MAKE) -C tests clean

install: cn_ws
	install -D cn_ws/cn_ws $(DESTDIR)$(prefix)/bin/cn_ws

.PHONY: all cn_ws tests
