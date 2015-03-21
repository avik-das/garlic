#include <inttypes.h>

typedef struct scm_native_export {
    char *name;
    void *fn;
} scm_native_export_t;

typedef void * scm_value_t;

#define NIL_VALUE   0
#define TRUE_VALUE  2
#define FALSE_VALUE 4

#define SCMVALUE_TO_INT(n) (((int64_t) n) >> 1)
#define INT_TO_SCMVALUE(n) ((scm_value_t) (((n) << 1) | 1))

scm_value_t scm_wrap_native(void *native_val);

const char * scm_unwrap_string(scm_value_t wrapped);
void * scm_unwrap_native(scm_value_t wrapped);
