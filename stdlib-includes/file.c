#include <garlic.h>
#include <stdio.h>
#include <stdlib.h>

garlic_value_t read_text(garlic_value_t filename) {
    FILE *file = fopen(garlic_unwrap_string(filename), "r");
    if (!file) {
        return garlic_empty_string;
    }

    // Go to the end of the file to figure out how many characters there are in
    // the file. Then, go back to the beginning of the file so it can be read.
    fseek(file, 0, SEEK_END);
    long size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Make sure to allocate space for the null-terminator at the end of the
    // contents string.
    char *contents = malloc(size + 1);
    if (!contents) {
        return garlic_empty_string;
    }

    fread(contents, size, 1, file);
    fclose(file);

    contents[size] = 0;
    return garlic_wrap_string(contents);
}

garlic_native_export_t file_exports[] = {
    {"read-text", read_text, 1},
    0
};
