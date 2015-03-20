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

scm_native_export_t libc_module_exports[] = {
    {"add", add},
    {"lastarg12", lastarg12},
    0
};
