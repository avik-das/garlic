%: %.s
	$(CC) -nostdlib -static $< -o $@
	strip $@

from-scratch: write-elf.rb
	./$<
	chmod +x ./$@

.PHONY: clean
clean:
	rm -f tiny
	rm -f from-scratch
