#include <garlic.h>
#include "../garlic-internal.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// XXX: does not support calling variadic because it is not known at runtime if
// a function is variadic. Sometimes, it seems to work, we should not depend on
// it always working.
garlic_value_t apply(garlic_value_t fn, garlic_value_t arglist) {
    int count = 0;
    garlic_value_t rest = arglist;
    while (rest != NIL_VALUE) {
        if (garlic_get_type(rest) != GARLIC_TYPE_CONS) {
            error_and_exit("ERROR - invalid argument list\n");
        }

        count++;
        rest = garlic_cdr(rest);
    }

    garlic_value_t *argarray =
        (garlic_value_t *) malloc(sizeof(garlic_value_t) * count);
    if (!argarray) {
        error_and_exit("unable to allocate memory for argument list");
    }

    rest = arglist;
    for (int i = 0; i < count; i++) {
        argarray[i] = garlic_car(rest);
        rest = garlic_cdr(rest);
    }

    garlic_value_t result = garlic_call_function(
            fn,
            argarray,
            count);

    free(argarray);
    return result;
}

garlic_value_t nullp(garlic_value_t value) {
    return value == NIL_VALUE ?
        (garlic_value_t) TRUE_VALUE :
        (garlic_value_t) FALSE_VALUE;
}

garlic_value_t symbolp(garlic_value_t value) {
    return garlic_get_type(value) == GARLIC_TYPE_ATOM ?
        TRUE_VALUE :
        FALSE_VALUE;
}

garlic_value_t listp(garlic_value_t value) {
    return garlic_get_type(value) == GARLIC_TYPE_CONS ?
        TRUE_VALUE :
        FALSE_VALUE;
}

garlic_value_t numberp(garlic_value_t value) {
    enum garlic_value_type type = garlic_get_type(value);
    return (type == GARLIC_TYPE_FIXNUM || type == GARLIC_TYPE_DOUBLE) ?
        TRUE_VALUE :
        FALSE_VALUE;
}

garlic_value_t sum(garlic_value_t vals) {
    int64_t intsum = 0;
    double dblsum = 0.0;

    int is_float = 0;

    while (vals != NIL_VALUE) {
        garlic_value_t val = garlic_car(vals);
        vals = garlic_cdr(vals);

        enum garlic_value_type type = garlic_get_type(val);

        if (type == GARLIC_TYPE_DOUBLE) {
            is_float = 1;
            dblsum += garlicval_to_double(val);
        } else if (type == GARLIC_TYPE_FIXNUM) {
            intsum += garlicval_to_int(val);
            dblsum += (double) garlicval_to_int(val);
        }
    }

    return is_float ?
        double_to_garlicval(dblsum) :
        int_to_garlicval(intsum);
}

garlic_value_t difference(garlic_value_t vals) {
    if (vals == NIL_VALUE) {
        // TODO: this will one day become a real error
        printf("ERROR: - called with too few arguments\n");
        return NIL_VALUE;
    }

    // If only one argument is passed in, then return that number negated.

    if (garlic_cdr(vals) == NIL_VALUE) {
        garlic_value_t raw_val = garlic_car(vals);

        enum garlic_value_type type = garlic_get_type(raw_val);

        if (type == GARLIC_TYPE_DOUBLE) {
            double raw_num = garlicval_to_double(raw_val);
            double neg_num = 0 - raw_num;
            return double_to_garlicval(neg_num);
        } else if (type == GARLIC_TYPE_FIXNUM) {
            int64_t raw_num = garlicval_to_int(raw_val);
            int64_t neg_num = 0 - raw_num;
            return int_to_garlicval(neg_num);
        }
    }

    // Otherwise, return the first number minus the sum of the rest of the
    // numbers.

    int64_t intdiff = 0;
    double dbldiff = 0.0;

    int is_float = 0;

    garlic_value_t raw_val = garlic_car(vals);
    enum garlic_value_type type = garlic_get_type(raw_val);

    if (type == GARLIC_TYPE_DOUBLE) {
        is_float = 1;
        // it doesn't matter what intdiff is
        dbldiff = garlicval_to_double(raw_val);
    } else if (type == GARLIC_TYPE_FIXNUM) {
        intdiff = garlicval_to_int(raw_val);
        dbldiff = (double) garlicval_to_int(raw_val);
    }

    vals = garlic_cdr(vals);

    while (vals != NIL_VALUE) {
        garlic_value_t val = garlic_car(vals);
        vals = garlic_cdr(vals);

        enum garlic_value_type type = garlic_get_type(val);

        if (type == GARLIC_TYPE_DOUBLE) {
            is_float = 1;
            dbldiff -= garlicval_to_double(val);
        } else if (type == GARLIC_TYPE_FIXNUM) {
            intdiff -= garlicval_to_int(val);
            dbldiff -= (double) garlicval_to_int(val);
        }
    }

    return is_float ?
        double_to_garlicval(dbldiff) :
        int_to_garlicval(intdiff);
}

garlic_value_t product(garlic_value_t vals) {
    int64_t intprod = 1;
    double dblprod = 1.0;

    int is_float = 0;

    while (vals != NIL_VALUE) {
        garlic_value_t val = garlic_car(vals);
        vals = garlic_cdr(vals);

        enum garlic_value_type type = garlic_get_type(val);

        if (type == GARLIC_TYPE_DOUBLE) {
            is_float = 1;
            dblprod *= garlicval_to_double(val);
        } else if (type == GARLIC_TYPE_FIXNUM) {
            intprod *= garlicval_to_int(val);
            dblprod *= (double) garlicval_to_int(val);
        }
    }

    return is_float ?
        double_to_garlicval(dblprod) :
        int_to_garlicval(intprod);
}

// This check applies to types that can be compared by identity, such as
// fixnums and symbols. This function won't check that all the values have the
// same type, because if they are identical, they already have the same type.
garlic_value_t equal_sign_identity(
        garlic_value_t reference,
        garlic_value_t vals) {
    while (vals != NIL_VALUE) {
        garlic_value_t item = garlic_car(vals);
        vals = garlic_cdr(vals);

        if (item != reference) {
            return FALSE_VALUE;
        }
    }

    return TRUE_VALUE;
}

garlic_value_t equal_sign(
        garlic_value_t a,
        garlic_value_t b,
        garlic_value_t rest) {
    garlic_value_t vals = rest;
    vals = garlic_make_cons(b, vals);
    vals = garlic_make_cons(a, vals);

    switch (garlic_get_type(a)) {
        case GARLIC_TYPE_FIXNUM:
        case GARLIC_TYPE_ATOM:
            return equal_sign_identity(a, vals);
        default:
            error_and_exit("ERROR: = can only compare numbers and symbols");

            // Control will not reach here, but compilers won't know that.
            // Return a bogus value just to avoid warnings at high warning
            // levels.
            return NIL_VALUE;
    }
}

garlic_value_t less_than(garlic_value_t a, garlic_value_t b) {
    enum garlic_value_type typea = garlic_get_type(a);
    enum garlic_value_type typeb = garlic_get_type(b);
    if (typea == GARLIC_TYPE_DOUBLE ||
            typeb == GARLIC_TYPE_DOUBLE) {
        // At least one of the two values is a float, so coerce both into
        // floats before comparing.
        double floata = typea == GARLIC_TYPE_DOUBLE
            ? garlicval_to_double(a)
            : (double) garlicval_to_int(a);
        double floatb = typeb == GARLIC_TYPE_DOUBLE
            ? garlicval_to_double(b)
            : (double) garlicval_to_int(b);
        return floata < floatb ? TRUE_VALUE : FALSE_VALUE;
    }

    // Both values are integers, so just compare them as ints. This ensures
    // that large integers don't lose precision by being cast as floats.
    return garlicval_to_int(a) < garlicval_to_int(b)
        ? TRUE_VALUE
        : FALSE_VALUE;
}

garlic_value_t greater_than(garlic_value_t a, garlic_value_t b) {
    enum garlic_value_type typea = garlic_get_type(a);
    enum garlic_value_type typeb = garlic_get_type(b);
    if (typea == GARLIC_TYPE_DOUBLE ||
            typeb == GARLIC_TYPE_DOUBLE) {
        // At least one of the two values is a float, so coerce both into
        // floats before comparing.
        double floata = typea == GARLIC_TYPE_DOUBLE
            ? garlicval_to_double(a)
            : (double) garlicval_to_int(a);
        double floatb = typeb == GARLIC_TYPE_DOUBLE
            ? garlicval_to_double(b)
            : (double) garlicval_to_int(b);
        return floata > floatb ? TRUE_VALUE : FALSE_VALUE;
    }

    // Both values are integers, so just compare them as ints. This ensures
    // that large integers don't lose precision by being cast as floats.
    return garlicval_to_int(a) > garlicval_to_int(b)
        ? TRUE_VALUE
        : FALSE_VALUE;
}

// The following bitwise operators are based on SRFI 151. See
// https://srfi.schemers.org/srfi-151/srfi-151.html
//
// If any listed operators have not been implemented, it's because they haven't
// been needed for practical purposes yet.

garlic_value_t bitwise_and(garlic_value_t a, garlic_value_t b) {
    if (garlic_get_type(a) != GARLIC_TYPE_FIXNUM ||
            garlic_get_type(b) != GARLIC_TYPE_FIXNUM) {
        error_and_exit("ERROR: bitwise-and can only be applied to integers");
    }

    return int_to_garlicval(garlicval_to_int(a) & garlicval_to_int(b));
}

garlic_value_t bitwise_ior(garlic_value_t a, garlic_value_t b) {
    if (garlic_get_type(a) != GARLIC_TYPE_FIXNUM ||
            garlic_get_type(b) != GARLIC_TYPE_FIXNUM) {
        error_and_exit("ERROR: bitwise-and can only be applied to integers");
    }

    // TODO: implement
    return int_to_garlicval(garlicval_to_int(a) | garlicval_to_int(b));
}

garlic_value_t arithmetic_shift(garlic_value_t a, garlic_value_t b) {
    if (garlic_get_type(a) != GARLIC_TYPE_FIXNUM ||
            garlic_get_type(b) != GARLIC_TYPE_FIXNUM) {
        error_and_exit(
                "ERROR: arithmetic_shift can only be applied to integers");
    }

    int64_t inta = garlicval_to_int(a);
    int64_t intb = garlicval_to_int(b);

         if (intb > 0) { return int_to_garlicval(inta <<  intb); }
    else if (intb < 0) { return int_to_garlicval(inta >> -intb); }
    else               { return a; }
}

garlic_value_t to_string(garlic_value_t val);

garlic_value_t cons_inner_to_string(garlic_value_t cons) {
    char *car_string_garlic_val = to_string(garlic_car(cons));
    const char *car_string = garlic_unwrap_string(car_string_garlic_val);
    size_t car_len = strlen(car_string);

    garlic_value_t cdr_val = garlic_cdr(cons);
    if (cdr_val == NIL_VALUE) {
        return car_string_garlic_val;
    } else if (garlic_get_type(cdr_val) == GARLIC_TYPE_CONS) {
        const char *cdr_string =
            garlic_unwrap_string(cons_inner_to_string(cdr_val));
        size_t cdr_len = strlen(cdr_string);

        size_t str_len = car_len + cdr_len + 2;
        char *str = malloc(sizeof(char) * str_len);
        snprintf(str, str_len, "%s %s", car_string, cdr_string);
        return garlic_wrap_string(str);
    } else {
        const char *cdr_string =
            garlic_unwrap_string(to_string(cdr_val));
        size_t cdr_len = strlen(cdr_string);

        size_t str_len = car_len + cdr_len + 4;
        char *str = malloc(sizeof(char) * str_len);
        snprintf(str, str_len, "%s . %s", car_string, cdr_string);
        return garlic_wrap_string(str);
    }
}

garlic_value_t to_string(garlic_value_t val) {
    enum garlic_value_type type = garlic_get_type(val);

    if (val == NIL_VALUE) {
        return garlic_wrap_string("()");
    } else if (val == TRUE_VALUE) {
        return garlic_wrap_string("#t");
    } else if (val == FALSE_VALUE) {
        return garlic_wrap_string("#f");
    } else if (type == GARLIC_TYPE_FIXNUM) {
        char *str = malloc(sizeof(char) * 50);
        snprintf(str, 50, "%" PRId64, garlicval_to_int(val));
        return garlic_wrap_string(str);
    } else if (type == GARLIC_TYPE_DOUBLE) {
        char *str = malloc(sizeof(char) * 50);
        snprintf(str, 50, "%f", garlicval_to_double(val));
        return garlic_wrap_string(str);
    } else if (type == GARLIC_TYPE_STRING) {
        return val;
    } else if (type == GARLIC_TYPE_LAMBDA) {
        char *str = malloc(sizeof(char) * 50);
        snprintf(str, 50, "#<fn: %p>", val);
        return garlic_wrap_string(str);
    } else if (type == GARLIC_TYPE_ATOM) {
        return garlic_wrap_string(garlic_atom_name(val));
    } else if (type == GARLIC_TYPE_CONS) {
        const char *inner = garlic_unwrap_string(cons_inner_to_string(val));
        size_t inner_len = strlen(inner);

        size_t str_len = inner_len + 3;
        char *str = malloc(sizeof(char) * str_len);
        snprintf(str, str_len, "(%s)", inner);
        return garlic_wrap_string(str);
        return garlic_wrap_string("TODO");
    } else if (type == GARLIC_TYPE_WRAPPED_NATIVE) {
        char *str = malloc(sizeof(char) * 50);
        snprintf(str, 50, "#<native: %p>", garlic_unwrap_native(val));
        return garlic_wrap_string(garlic_atom_name(val));
    }
}

garlic_value_t vals_to_concatenated_string(garlic_value_t vals) {
    size_t numvals = 0;
    garlic_value_t vals_to_count = vals;
    while (vals_to_count != NIL_VALUE) {
        numvals++;
        vals_to_count = garlic_cdr(vals_to_count);
    }

    garlic_value_t *vals_as_strings =
        (garlic_value_t *) malloc(sizeof(garlic_value_t) * numvals);
    if (!vals_as_strings) {
        error_and_exit("unable to allocate memory for string concatenation");
    }

    int64_t i = 0;
    while (vals != NIL_VALUE) {
        garlic_value_t val = garlic_car(vals);
        vals_as_strings[i] = to_string(val);

        i++;
        vals = garlic_cdr(vals);
    }

    garlic_value_t string_list = NIL_VALUE;
    for (i = i - 1; i >= 0; i--) {
        string_list = garlic_make_cons(vals_as_strings[i], string_list);
    }

    return garlic_internal_string_concat(string_list);
}

garlic_value_t display(garlic_value_t vals) {
    garlic_value_t msg = vals_to_concatenated_string(vals);
    printf("%s", garlic_unwrap_string(msg));

    return NIL_VALUE;
}

garlic_value_t error_and_exit_from_garlic(garlic_value_t msgs) {
    garlic_value_t msg = vals_to_concatenated_string(msgs);
    error_and_exit(garlic_unwrap_string(msg));
}

// Many of the functions are lifted straight from the runtime, and so, they do
// not need to be re-implemented here.
garlic_native_export_t core_exports[] = {
    {"apply", apply, 2},

    {"null?", nullp, 1},
    {"symbol?", symbolp, 1},
    {"list?", listp, 1},
    {"number?", numberp, 1},

    {"cons", garlic_make_cons, 2},
    {"car", garlic_car, 1},
    {"cdr", garlic_cdr, 1},
    {"append-in-place", garlic_internal_append_in_place, 2, 1},

    {"+", sum, 0, 1},
    {"-", difference, 0, 1},
    {"*", product, 0, 1},
    {"=", equal_sign, 2, 1},

    {"<", less_than, 2},
    {">", greater_than, 2},

    {"bitwise-and", bitwise_and, 2},
    {"bitwise-ior", bitwise_ior, 2},
    {"arithmetic-shift", arithmetic_shift, 2},

    {"display", display, 0, 1},

    {"error-and-exit", error_and_exit_from_garlic, 0, 1},

    0
};
