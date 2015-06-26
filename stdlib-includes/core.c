#include <stdio.h>
#include <string.h>
#include <garlic.h>

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

garlic_value_t sum(garlic_value_t vals) {
    int64_t sum = 0;

    while (vals != NIL_VALUE) {
        garlic_value_t val = garlic_car(vals);
        vals = garlic_cdr(vals);

        sum += garlicval_to_int(val);
    }

    return int_to_garlicval(sum);
}

garlic_value_t difference(garlic_value_t vals) {
    if (vals == NIL_VALUE) {
        // TODO: this will one day become a real error
        printf("ERROR: - called with too few arguments\n");
        return NIL_VALUE;
    }

    // If only one argument is passed in, then return that number negated.

    if (garlic_cdr(vals) == NIL_VALUE) {
        int64_t raw_num = garlicval_to_int(garlic_car(vals));
        int64_t neg_num = 0 - raw_num;
        return int_to_garlicval(neg_num);
    }

    // Otherwise, return the first number minus the sum of the rest of the
    // numbers.

    int64_t difference = garlicval_to_int(garlic_car(vals));
    vals = garlic_cdr(vals);

    while (vals != NIL_VALUE) {
        garlic_value_t val = garlic_car(vals);
        vals = garlic_cdr(vals);

        difference -= garlicval_to_int(val);
    }

    return int_to_garlicval(difference);
}

garlic_value_t product(garlic_value_t vals) {
    int64_t prod = 1;

    while (vals != NIL_VALUE) {
        garlic_value_t val = garlic_car(vals);
        vals = garlic_cdr(vals);

        prod *= garlicval_to_int(val);
    }

    return int_to_garlicval(prod);
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
            printf("ERROR: = can only compare numbers and symbols");
            return NIL_VALUE;
    }
}

void display_single(garlic_value_t val);

void display_cons_inner(garlic_value_t cons) {
    display_single(garlic_car(cons));

    garlic_value_t cdr_val = garlic_cdr(cons);
    if (cdr_val == NIL_VALUE) {
        // Do nothing
    } else if (garlic_get_type(cdr_val) == GARLIC_TYPE_CONS) {
        printf(" ");
        display_cons_inner(cdr_val);
    } else {
        printf(" . ");
        display_single(cdr_val);
    }
}

void display_single(garlic_value_t val) {
    enum garlic_value_type type = garlic_get_type(val);

    if (val == NIL_VALUE) {
        printf("()");
    } else if (val == TRUE_VALUE) {
        printf("#t");
    } else if (val == FALSE_VALUE) {
        printf("#f");
    } else if (type == GARLIC_TYPE_FIXNUM) {
        printf("%" PRId64, garlicval_to_int(val));
    } else if (type == GARLIC_TYPE_STRING) {
        printf("%s", garlic_unwrap_string(val));
    } else if (type == GARLIC_TYPE_LAMBDA) {
        printf("#<fn: %p>", val);
    } else if (type == GARLIC_TYPE_ATOM) {
        printf("%s", garlic_atom_name(val));
    } else if (type == GARLIC_TYPE_CONS) {
        printf("(");
        display_cons_inner(val);
        printf(")");
    } else if (type == GARLIC_TYPE_WRAPPED_NATIVE) {
        printf("#<native: %p>", garlic_unwrap_native(val));
    }
}

garlic_value_t display(garlic_value_t vals) {
    while (vals != NIL_VALUE) {
        garlic_value_t item = garlic_car(vals);
        vals = garlic_cdr(vals);

        display_single(item);
    }

    return NIL_VALUE;
}

// Many of the functions are lifted straight from the runtime, and so, they do
// not need to be re-implemented here.
garlic_native_export_t core_exports[] = {
    {"null?", nullp, 1},
    {"symbol?", symbolp, 1},

    {"cons", garlic_make_cons, 2},
    {"car", garlic_car, 1},
    {"cdr", garlic_cdr, 1},

    {"+", sum, 0, 1},
    {"-", difference, 0, 1},
    {"*", product, 0, 1},
    {"=", equal_sign, 2, 1},

    {"display", display, 0, 1},
    0
};
