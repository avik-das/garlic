#include <inttypes.h>

typedef struct garlic_native_export {
    char *name;
    void *fn;
    unsigned int arity;
} garlic_native_export_t;

typedef void * garlic_value_t;

#define NIL_VALUE   0
#define TRUE_VALUE  2
#define FALSE_VALUE 4

/* Convert a value of type garlic_value_t into an int64_t. */
#define garlicval_to_int(n) (((int64_t) n) >> 1)

/* Convert an int64_t into a value of type garlic_value_t. */
#define int_to_garlicval(n) ((garlic_value_t) (((n) << 1) | 1))

/* Wrap a pointer into a structure that can be passed to Garlic. This value
 * will not be usable in Garlic, but it can be passed back to the C module and
 * unwrapped using "garlic_unwrap_string" to get back the original pointer.
 *
 * This is so a native value can be passed around between multiple functions.
 */
garlic_value_t garlic_wrap_native(void *native_val);

/* Unwrap a value of type garlic_value_t to get back the original native
 * pointer.  It is assumed the pointer was initially wrapped using
 * "garlic_wrap_native". */
void * garlic_unwrap_native(garlic_value_t wrapped);

/* Given a value of type garlic_value_t, get back the C string contained
 * within. */
const char * garlic_unwrap_string(garlic_value_t wrapped);
