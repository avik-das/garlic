/**
 * A set of utilities for the recursive compiler that are just here for
 * quick-and-dirty testing. Will be removed even faster than the utilities in
 * compiler_utils.c.
 */

#include <garlic.h>

#include <stdio.h>

/**
 * Function that prints the given list of bytes to the standard output. Assumes
 * the bytes are provided in one shot, allowing this function to break up the
 * bytes into manageable chunks for pretty-printing.
 */
garlic_value_t print_bytes(garlic_value_t bytes) {
    int i = 0;
    while (bytes != NIL_VALUE) {
        int64_t byte = garlicval_to_int(garlic_car(bytes));
        bytes = garlic_cdr(bytes);

        printf("%02x ", byte);

        if (i % 16 == 3 || i % 16 == 11) { printf(" "); }
        if (i % 16 == 7)                 { printf("  "); }
        if (i % 16 == 15)                { printf("\n"); }

        i++;
    }

    printf("\n");

    return NIL_VALUE;
}

garlic_native_export_t compiler_temp_exports[] = {
    {"print-bytes", print_bytes, 1},
    0
};
