#include <inttypes.h>

typedef struct scm_native_export {
    char *name;
    void *fn;
    unsigned int arity;
} scm_native_export_t;

typedef void * scm_value_t;

#define NIL_VALUE   0
#define TRUE_VALUE  2
#define FALSE_VALUE 4

/* Convert a value of type scm_value_t into an int64_t. */
#define SCMVALUE_TO_INT(n) (((int64_t) n) >> 1)

/* Convert an int64_t into a value of type scm_value_t. */
#define INT_TO_SCMVALUE(n) ((scm_value_t) (((n) << 1) | 1))

/* Wrap a pointer into a structure that can be passed to Garlic. This value
 * will not be usable in Garlic, but it can be passed back to the C module and
 * unwrapped using "scm_unwrap_string" to get back the original pointer.
 *
 * This is so a native value can be passed around between multiple functions.
 */
scm_value_t scm_wrap_native(void *native_val);

/* Unwrap a value of type scm_value_t to get back the original native pointer.
 * It is assumed the pointer was initially wrapped using "scm_wrap_native". */
void * scm_unwrap_native(scm_value_t wrapped);

/* Given a value of type scm_value_t, get back the C string contained within.
 */
const char * scm_unwrap_string(scm_value_t wrapped);
