%: %.s
	# Don't generate a GNU build-id section
	$(CC) -nostdlib -static $< -o $@ -Wl,--build-id=none
	strip $@

from-scratch: write-elf.rb
	./$<
	chmod +x ./$@

.PHONY: clean
clean:
	rm -f tiny
	rm -f from-scratch
	rm -f from-scratch-write
