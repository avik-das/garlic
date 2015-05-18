#include <inttypes.h>

typedef struct garlic_native_export {
    char *name;
    void *fn;
    unsigned int arity;
    unsigned int variadic;
} garlic_native_export_t;

typedef void * garlic_value_t;

enum garlic_value_type {
    GARLIC_TYPE_NIL,
    GARLIC_TYPE_BOOLEAN,
    GARLIC_TYPE_FIXNUM,
    GARLIC_TYPE_LAMBDA,
    GARLIC_TYPE_ATOM,
    GARLIC_TYPE_STRING,
    GARLIC_TYPE_CONS,
    GARLIC_TYPE_WRAPPED_NATIVE
};

/* Determine the type of a alue of type garlic_value_t. */
enum garlic_value_type garlic_get_type(garlic_value_t val);

#define NIL_VALUE   ((garlic_value_t) 0)
#define TRUE_VALUE  ((garlic_value_t) 2)
#define FALSE_VALUE ((garlic_value_t) 4)

/* Convert a value of type garlic_value_t into an int64_t. */
int64_t garlicval_to_int(garlic_value_t n);

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

/* Returns a C-string representation of an atom's name. Normally, the name
 * should not be used, since the point of an atom is that all occurences of a
 * specific atom are stored in the same location in memory. However, getting
 * the name is useful for purposes such as displaying the name. */
const char * garlic_atom_name(garlic_value_t atom);

/* Create a cons cell with the given elements. */
garlic_value_t garlic_make_cons(garlic_value_t car_val, garlic_value_t cdr_val);
/* Retrieve the first element of a cons cell. */
garlic_value_t garlic_car(garlic_value_t cons_val);
/* Retrieve the second element of a cons cell. */
garlic_value_t garlic_cdr(garlic_value_t cons_val);
