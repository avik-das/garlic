#include <inttypes.h>

typedef struct scm_native_export {
    char *name;
    void *fn;
} scm_native_export_t;

typedef void * scm_value_t;

#define SCMVALUE_TO_INT(n) (((int64_t) n) >> 1)
#define INT_TO_SCMVALUE(n) ((scm_value_t) (((n) << 1) | 1))
