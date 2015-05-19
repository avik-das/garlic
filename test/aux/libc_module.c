#include <stdio.h>
#include <garlic.h>

// This function will not be included as part of the exports, so it will not be
// accessible outside this module.
garlic_value_t private_identity(garlic_value_t input) {
    return input;
}

garlic_value_t add(garlic_value_t a, garlic_value_t b) {
    printf("Adding %" PRId64 " and %" PRId64 " in C!\n",
            garlicval_to_int(a), garlicval_to_int(b));

    return private_identity(
            int_to_garlicval(garlicval_to_int(a) + garlicval_to_int(b))
            );
}

garlic_value_t lastarg12(
        garlic_value_t a,
        garlic_value_t b,
        garlic_value_t c,
        garlic_value_t d,
        garlic_value_t e,
        garlic_value_t f,
        garlic_value_t g,
        garlic_value_t h,
        garlic_value_t i,
        garlic_value_t j,
        garlic_value_t k,
        garlic_value_t l
        ) {
    printf("Returning 12th argument from C!\n");
    return l;
}

/* A series of functions with different arities, useful for testing various
 * scenarios regarding passing arguments via registers. */

garlic_value_t arg0() {
    return int_to_garlicval(0);
}

garlic_value_t arg1(garlic_value_t a) {
    int64_t ca = garlicval_to_int(a);
    printf("%" PRId64 " = ", ca);

    return a;
}

garlic_value_t arg2(
        garlic_value_t a,
        garlic_value_t b) {
    int64_t ca = garlicval_to_int(a);
    int64_t cb = garlicval_to_int(b);

    printf("%" PRId64
            " + %" PRId64 " = ",
            ca, cb);

    int64_t sum = ca + cb;
    return int_to_garlicval(sum);
}

garlic_value_t arg3(
        garlic_value_t a,
        garlic_value_t b,
        garlic_value_t c) {
    int64_t ca = garlicval_to_int(a);
    int64_t cb = garlicval_to_int(b);
    int64_t cc = garlicval_to_int(c);

    printf("%" PRId64
            " + %" PRId64
            " + %" PRId64 " = ",
            ca, cb, cc);

    int64_t sum = ca + cb + cc;
    return int_to_garlicval(sum);
}

garlic_value_t arg4(
        garlic_value_t a,
        garlic_value_t b,
        garlic_value_t c,
        garlic_value_t d) {
    int64_t ca = garlicval_to_int(a);
    int64_t cb = garlicval_to_int(b);
    int64_t cc = garlicval_to_int(c);
    int64_t cd = garlicval_to_int(d);

    printf("%" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64 " = ",
            ca, cb, cc, cd);

    int64_t sum = ca + cb + cc + cd;
    return int_to_garlicval(sum);
}

garlic_value_t arg5(
        garlic_value_t a,
        garlic_value_t b,
        garlic_value_t c,
        garlic_value_t d,
        garlic_value_t e) {
    int64_t ca = garlicval_to_int(a);
    int64_t cb = garlicval_to_int(b);
    int64_t cc = garlicval_to_int(c);
    int64_t cd = garlicval_to_int(d);
    int64_t ce = garlicval_to_int(e);

    printf("%" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64 " = ",
            ca, cb, cc, cd, ce);

    int64_t sum = ca + cb + cc + cd + ce;
    return int_to_garlicval(sum);
}

garlic_value_t arg6(
        garlic_value_t a,
        garlic_value_t b,
        garlic_value_t c,
        garlic_value_t d,
        garlic_value_t e,
        garlic_value_t f) {
    int64_t ca = garlicval_to_int(a);
    int64_t cb = garlicval_to_int(b);
    int64_t cc = garlicval_to_int(c);
    int64_t cd = garlicval_to_int(d);
    int64_t ce = garlicval_to_int(e);
    int64_t cf = garlicval_to_int(f);

    printf("%" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64 " = ",
            ca, cb, cc, cd, ce, cf);

    int64_t sum = ca + cb + cc + cd + ce + cf;
    return int_to_garlicval(sum);
}

// More than 6 parameters means spilling onto the stack.
garlic_value_t arg8(
        garlic_value_t a,
        garlic_value_t b,
        garlic_value_t c,
        garlic_value_t d,
        garlic_value_t e,
        garlic_value_t f,
        garlic_value_t g,
        garlic_value_t h) {
    int64_t ca = garlicval_to_int(a);
    int64_t cb = garlicval_to_int(b);
    int64_t cc = garlicval_to_int(c);
    int64_t cd = garlicval_to_int(d);
    int64_t ce = garlicval_to_int(e);
    int64_t cf = garlicval_to_int(f);
    int64_t cg = garlicval_to_int(g);
    int64_t ch = garlicval_to_int(h);

    printf("%" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64 " = ",
            ca, cb, cc, cd, ce, cf, cg, ch);

    int64_t sum = ca + cb + cc + cd + ce + cf + cg + ch;
    return int_to_garlicval(sum);
}

garlic_value_t callme5(garlic_value_t fn) {
    garlic_value_t args[] = {
        int_to_garlicval(1),
        int_to_garlicval(2),
        int_to_garlicval(3),
        int_to_garlicval(4),
        int_to_garlicval(5)
    };

    return garlic_call_function(fn, args, 5);
}

garlic_value_t callme6(garlic_value_t fn) {
    garlic_value_t args[] = {
        int_to_garlicval(1),
        int_to_garlicval(2),
        int_to_garlicval(3),
        int_to_garlicval(4),
        int_to_garlicval(5),
        int_to_garlicval(6)
    };

    return garlic_call_function(fn, args, 6);
}

garlic_native_export_t libc_module_exports[] = {
    {"add", add, 2},
    {"lastarg12", lastarg12, 12},
    {"arg0", arg0, 0},
    {"arg1", arg1, 1},
    {"arg2", arg2, 2},
    {"arg3", arg3, 3},
    {"arg4", arg4, 4},
    {"arg5", arg5, 5},
    {"arg6", arg6, 6},
    {"arg8", arg8, 8},
    {"callme5", callme5, 1},
    {"callme6", callme6, 1},
    0
};
