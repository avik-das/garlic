all:
	@echo "USAGE: make (linux | osx)"

.PHONY: linux osx clean

linux:
	../s-expr.rb marley.scm -- -lSDL2 -lSDL2_image

osx:
	../s-expr.rb marley.scm -- -F ~/Library/Frameworks -framework SDL2 -framework SDL2_image

clean:
	rm -rf build main main.dSYM
