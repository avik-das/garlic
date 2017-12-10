#include <garlic.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

garlic_value_t concat(garlic_value_t args) {
    if (args == NIL_VALUE) {
        return garlic_empty_string;
    }

    garlic_value_t args_copy = args;

    size_t total_length = 0;
    while (args_copy != NIL_VALUE) {
        garlic_value_t item = garlic_car(args_copy);
        args_copy = garlic_cdr(args_copy);

        if (garlic_get_type(item) != GARLIC_TYPE_STRING) {
            printf("non-string passed to concat\n");
            return NIL_VALUE;
        }

        total_length += garlic_string_length(item);
    }

    total_length++; // for the NULL-terminator

    char *result = (char *) malloc(sizeof(char) * total_length);
    size_t start = 0;
    args_copy = args;
    while (args_copy != NIL_VALUE) {
        garlic_value_t item = garlic_car(args_copy);
        args_copy = garlic_cdr(args_copy);

        const char *str = garlic_unwrap_string(item);
        strcpy(result + start, str);
        start += garlic_string_length(item);
    }

    return garlic_wrap_string(result);
}

garlic_value_t concat_list(garlic_value_t list) {
    if (list != NIL_VALUE &&
            garlic_get_type(list) != GARLIC_TYPE_CONS) {
        printf("non-list passed to concat-list\n");
        return NIL_VALUE;
    }

    return concat(list);
}

garlic_value_t symbol_to_str(garlic_value_t sym) {
    if (garlic_get_type(sym) != GARLIC_TYPE_ATOM) {
        printf("non-symbol passed to symbol->str\n");
        return NIL_VALUE;
    }

    return garlic_wrap_string(garlic_atom_name(sym));
}

garlic_value_t string_equalp(garlic_value_t str1, garlic_value_t str2) {
    if (garlic_get_type(str1) != GARLIC_TYPE_STRING ||
            garlic_get_type(str2) != GARLIC_TYPE_STRING) {
        error_and_exit("ERROR - string=? can only compare strings");
    }

    const char *cstr1 = garlic_unwrap_string(str1);
    const char *cstr2 = garlic_unwrap_string(str2);
    return strcmp(cstr1, cstr2) == 0 ? TRUE_VALUE : FALSE_VALUE;
}

garlic_native_export_t string_exports[] = {
    {"concat", concat, 0, 1},
    {"concat-list", concat_list, 1},
    {"symbol->str", symbol_to_str, 1},
    {"string=?", string_equalp, 2},
    0
};
