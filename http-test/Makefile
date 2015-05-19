CFLAGS = -lmicrohttpd

server: server.scm
	../garlic -o $@ $< -- $(CFLAGS)

.PHONY: clean

clean:
	rm -rf build server server.dSYM
