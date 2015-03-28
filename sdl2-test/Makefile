all:
	@# TODO: support linux
	@echo "USAGE: make osx"

.PHONY: osx clean

osx:
	../s-expr.rb marley.scm -- -F ~/Library/Frameworks -framework SDL2 -framework SDL2_image

clean:
	rm -rf build main main.dSYM
