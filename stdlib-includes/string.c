#include <garlic.h>
#include "../garlic-internal.h"

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

static garlic_value_t nullp(garlic_value_t str) {
    if (garlic_get_type(str) != GARLIC_TYPE_STRING) {
        error_and_exit("ERROR - string:nullp - value is not a string");
    }

    return garlic_string_length(str) == 0 ?  TRUE_VALUE : FALSE_VALUE;
}

garlic_value_t concat_list(garlic_value_t list) {
    if (list != NIL_VALUE &&
            garlic_get_type(list) != GARLIC_TYPE_CONS) {
        printf("non-list passed to concat-list\n");
        return NIL_VALUE;
    }

    return garlic_internal_string_concat(list);
}

garlic_value_t string_tail(garlic_value_t str, garlic_value_t index) {
    if (garlic_get_type(str) != GARLIC_TYPE_STRING) {
        error_and_exit("ERROR - string:string-tail - value is not a string");
    }

    if (garlic_get_type(index) != GARLIC_TYPE_FIXNUM) {
        error_and_exit("ERROR - string:string-tail - index is not an integer");
    }

    int64_t i = garlicval_to_int(index);

    if (i == 0) {
        return str;
    }

    if (i >= garlic_string_length(str)) {
        return garlic_empty_string;
    }

    return garlic_wrap_string(garlic_unwrap_string(str) + i);
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

garlic_value_t character_at(garlic_value_t str, garlic_value_t index) {
    if (garlic_get_type(str) != GARLIC_TYPE_STRING) {
        error_and_exit("ERROR - string:at - value is not a string");
    }

    if (garlic_get_type(index) != GARLIC_TYPE_FIXNUM) {
        error_and_exit("ERROR - string:at - index is not an integer");
    }

    int64_t i = garlicval_to_int(index);
    if (i >= garlic_string_length(str)) {
        return NIL_VALUE;
    }

    char *result = (char *) malloc(sizeof(char) * 2);
    result[0] = garlic_unwrap_string(str)[i];
    result[1] = 0;
    return garlic_wrap_string(result);
}

garlic_value_t downcase(garlic_value_t str) {
    if (garlic_get_type(str) != GARLIC_TYPE_STRING) {
        error_and_exit("ERROR - string:downcase - value is not a string");
    }

    const char *cstr = garlic_unwrap_string(str);
    size_t len = garlic_string_length(str);

    char *result = (char *) malloc(sizeof(char) * (len + 1));

    for (int i = 0; i < len; i++) {
        result[i] = tolower(cstr[i]);
    }

    result[len] = 0;
    return garlic_wrap_string(result);
}

garlic_native_export_t string_exports[] = {
    {"null?", nullp, 1},
    {"concat", garlic_internal_string_concat, 0, 1},
    {"concat-list", concat_list, 1},
    {"string-tail", string_tail, 2},
    {"symbol->str", symbol_to_str, 1},
    {"string=?", string_equalp, 2},
    {"at", character_at, 2},
    {"downcase", downcase, 1},
    0
};
