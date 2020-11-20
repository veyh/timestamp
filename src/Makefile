# OH, HAI! :)

PREFIX    ?= /usr/local
BINDIR    ?= $(DESTDIR)$(PREFIX)/bin/
MANDIR    ?= $(DESTDIR)$(PREFIX)/share/man/man1/

FILTERS    = timestamp stampit
SCDOCS    := $(wildcard *.1.md)
MANPAGES  := $(SCDOCS:.1.md=.1)
ZMANPAGES := $(MANPAGES:.1=.1.gz)

GZIP       = gzip
SCDOC      = scdoc
INSTALL    = install

.PHONY: all
all: $(FILTERS) $(MANPAGES)

install: $(FILTERS) $(ZMANPAGES)
	@test -d $(BINDIR) || mkdir -p $(BINDIR)
	$(INSTALL) -m 0755 $(FILTERS) $(BINDIR)
	@test -d $(MANDIR) || mkdir -p $(MANDIR)
	$(INSTALL) -m 0644 $(ZMANPAGES) $(MANDIR)

%.1: %.1.md
	@$(SCDOC) <$< >$@

%.1.gz: %.1
	@$(GZIP) -c $< >$@

.PHONY: mostlyclean distclean clean
mostlyclean:
	@rm -vf *~
clean: mostlyclean
	@rm -vf $(FILTERS) $(ZMANPAGES)
distclean: clean
	@rm -vf $(MANPAGES)

# eof
