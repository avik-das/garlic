SOURCES := $(foreach dir,., $(notdir $(wildcard $(dir)/*.scm)))
OUTPUT  := garlic

$(OUTPUT): garlic.scm $(SOURCES)
	../garlic -o $@ $<

.PHONY: clean

clean:
	rm -rf build $(OUTPUT) $(OUTPUT).dSYM
