timestamp: timestamp.c

install: timestamp timestamp.1
	@/usr/bin/install -D --strip timestamp "$(DESTDIR)/usr/bin/timestamp"
	@/usr/bin/install -D --mode=644 timestamp.1 "$(DESTDIR)/usr/share/man/man1/timestamp.1"

clean:
	@/bin/rm -f timestamp
