#include <garlic.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

/**
 * Writes the given list of bytes to the specified file. Will overwrite any
 * contents in the file of the given filename, if present.
 */
garlic_value_t write_bytes(garlic_value_t filename, garlic_value_t bytes) {
    const char *c_filename = garlic_unwrap_string(filename);
    FILE *file = fopen(c_filename, "wb");
    if (!file) {
        char *err = malloc(sizeof(char) * (50 + strlen(c_filename)));
        strcpy(err, "could not open file for writing: ");
        strcat(err, c_filename);

        error_and_exit(err);
    }

    size_t num_bytes = 0;
    garlic_value_t bytes_to_count = bytes;
    while (bytes_to_count != NIL_VALUE) {
        num_bytes++;
        bytes_to_count = garlic_cdr(bytes_to_count);
    }

    int8_t *raw_bytes = malloc(sizeof(int8_t) * num_bytes);
    if (!raw_bytes) {
        error_and_exit("unable to allocate memory for bytes to write");
    }

    size_t i = 0;
    while (bytes != NIL_VALUE) {
        int64_t byte = garlicval_to_int(garlic_car(bytes));
        bytes = garlic_cdr(bytes);

        raw_bytes[i] = (int8_t) (byte & 0xFF);
        i++;
    }

    fwrite(raw_bytes, sizeof(int8_t), num_bytes, file);
    fclose(file);

    return NIL_VALUE;
}

garlic_native_export_t file_exports[] = {
    {"read-text", read_text, 1},
    {"write-bytes", write_bytes, 2},
    0
};
