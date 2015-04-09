#include <stdio.h>
#include <scm.h>

scm_value_t add(scm_value_t a, scm_value_t b) {
    printf("Adding %" PRId64 " and %" PRId64 " in C!\n",
            SCMVALUE_TO_INT(a), SCMVALUE_TO_INT(b));

    return INT_TO_SCMVALUE(SCMVALUE_TO_INT(a) + SCMVALUE_TO_INT(b));
}

scm_value_t lastarg12(
        scm_value_t a,
        scm_value_t b,
        scm_value_t c,
        scm_value_t d,
        scm_value_t e,
        scm_value_t f,
        scm_value_t g,
        scm_value_t h,
        scm_value_t i,
        scm_value_t j,
        scm_value_t k,
        scm_value_t l
        ) {
    printf("Returning 12th argument from C!\n");
    return l;
}

/* A series of functions with different arities, useful for testing various
 * scenarios regarding passing arguments via registers. */

scm_value_t arg0() {
    return INT_TO_SCMVALUE(0);
}

scm_value_t arg1(scm_value_t a) {
    int64_t ca = SCMVALUE_TO_INT(a);
    printf("%" PRId64 " = ", ca);

    return a;
}

scm_value_t arg2(
        scm_value_t a,
        scm_value_t b) {
    int64_t ca = SCMVALUE_TO_INT(a);
    int64_t cb = SCMVALUE_TO_INT(b);

    printf("%" PRId64
            " + %" PRId64 " = ",
            ca, cb);

    int64_t sum = ca + cb;
    return INT_TO_SCMVALUE(sum);
}

scm_value_t arg3(
        scm_value_t a,
        scm_value_t b,
        scm_value_t c) {
    int64_t ca = SCMVALUE_TO_INT(a);
    int64_t cb = SCMVALUE_TO_INT(b);
    int64_t cc = SCMVALUE_TO_INT(c);

    printf("%" PRId64
            " + %" PRId64
            " + %" PRId64 " = ",
            ca, cb, cc);

    int64_t sum = ca + cb + cc;
    return INT_TO_SCMVALUE(sum);
}

scm_value_t arg4(
        scm_value_t a,
        scm_value_t b,
        scm_value_t c,
        scm_value_t d) {
    int64_t ca = SCMVALUE_TO_INT(a);
    int64_t cb = SCMVALUE_TO_INT(b);
    int64_t cc = SCMVALUE_TO_INT(c);
    int64_t cd = SCMVALUE_TO_INT(d);

    printf("%" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64 " = ",
            ca, cb, cc, cd);

    int64_t sum = ca + cb + cc + cd;
    return INT_TO_SCMVALUE(sum);
}

scm_value_t arg5(
        scm_value_t a,
        scm_value_t b,
        scm_value_t c,
        scm_value_t d,
        scm_value_t e) {
    int64_t ca = SCMVALUE_TO_INT(a);
    int64_t cb = SCMVALUE_TO_INT(b);
    int64_t cc = SCMVALUE_TO_INT(c);
    int64_t cd = SCMVALUE_TO_INT(d);
    int64_t ce = SCMVALUE_TO_INT(e);

    printf("%" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64 " = ",
            ca, cb, cc, cd, ce);

    int64_t sum = ca + cb + cc + cd + ce;
    return INT_TO_SCMVALUE(sum);
}

scm_value_t arg6(
        scm_value_t a,
        scm_value_t b,
        scm_value_t c,
        scm_value_t d,
        scm_value_t e,
        scm_value_t f) {
    int64_t ca = SCMVALUE_TO_INT(a);
    int64_t cb = SCMVALUE_TO_INT(b);
    int64_t cc = SCMVALUE_TO_INT(c);
    int64_t cd = SCMVALUE_TO_INT(d);
    int64_t ce = SCMVALUE_TO_INT(e);
    int64_t cf = SCMVALUE_TO_INT(f);

    printf("%" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64
            " + %" PRId64 " = ",
            ca, cb, cc, cd, ce, cf);

    int64_t sum = ca + cb + cc + cd + ce + cf;
    return INT_TO_SCMVALUE(sum);
}

// More than 6 parameters means spilling onto the stack.
scm_value_t arg8(
        scm_value_t a,
        scm_value_t b,
        scm_value_t c,
        scm_value_t d,
        scm_value_t e,
        scm_value_t f,
        scm_value_t g,
        scm_value_t h) {
    int64_t ca = SCMVALUE_TO_INT(a);
    int64_t cb = SCMVALUE_TO_INT(b);
    int64_t cc = SCMVALUE_TO_INT(c);
    int64_t cd = SCMVALUE_TO_INT(d);
    int64_t ce = SCMVALUE_TO_INT(e);
    int64_t cf = SCMVALUE_TO_INT(f);
    int64_t cg = SCMVALUE_TO_INT(g);
    int64_t ch = SCMVALUE_TO_INT(h);

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
    return INT_TO_SCMVALUE(sum);
}

scm_native_export_t libc_module_exports[] = {
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
    0
};
