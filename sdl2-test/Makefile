OS = $(shell uname)

ifeq "$(OS)" "Darwin"
CFLAGS = -F ~/Library/Frameworks -framework SDL2 -framework SDL2_image
else # Linux
CFLAGS = -lSDL2 -lSDL2_image
endif

marley: marley.scm
	../garlic -o $@ $< -- $(CFLAGS)

.PHONY: clean

clean:
	rm -rf build marley marley.dSYM
