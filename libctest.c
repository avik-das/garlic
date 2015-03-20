#include <stdio.h>

// TODO: move this to a standard header
#include <inttypes.h>

typedef struct scm_native_export {
    char *name;
    void *fn;
} scm_native_export_t;

typedef void * scm_value_t;

enum scm_value_type {
    SCM_TYPE_LAMBDA = 0,
    SCM_TYPE_ATOM,
    SCM_TYPE_STRING,
    SCM_TYPE_CONS
};

struct scm_value {
    enum scm_value_type type;
};

struct scm_cons {
    struct scm_value super;
    scm_value_t car;
    scm_value_t cdr;
};

#define INT_TO_FIXNUM(n) (((n) << 1) | 1)
#define FIXNUM_TO_INT(n) ((n) >> 1)

int64_t add(int64_t a, int64_t b) {
    printf("Adding %" PRId64 " and %" PRId64 " in C!\n",
            FIXNUM_TO_INT(a), FIXNUM_TO_INT(b));

    return INT_TO_FIXNUM(FIXNUM_TO_INT(a) + FIXNUM_TO_INT(b));
}

scm_native_export_t libctest_exports[] = {
    {"add", add},
    0
};
