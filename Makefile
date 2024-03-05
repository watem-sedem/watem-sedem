SUBDIRS := $(wildcard */.)

all: tests watem_sedem

watem_sedem:
	$(MAKE) -C watem_sedem

tests:
	$(MAKE) -C watem_sedem
	$(MAKE) -C tests

integration_test:
	$(MAKE) -C watem_sedem
	$(MAKE) -C tests
	testfiles/test.sh
	pytest testfiles

clean:
	$(MAKE) -C watem_sedem clean
	$(MAKE) -C tests clean

install: watem_sedem
	install -D watem_sedem/watem_sedem $(DESTDIR)$(prefix)/bin/watem_sedem

.PHONY: all watem_sedem tests
