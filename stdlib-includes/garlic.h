#ifndef GARLIC_H
#define GARLIC_H

#include <inttypes.h>
#include <stddef.h>

typedef struct garlic_native_export {
    char *name;
    void *fn;
    unsigned int arity;
    unsigned int variadic;
} garlic_native_export_t;

/* Print out the given message and exit. */
void error_and_exit(const char *message);

typedef void * garlic_value_t;

enum garlic_value_type {
    GARLIC_TYPE_NIL,
    GARLIC_TYPE_BOOLEAN,
    GARLIC_TYPE_FIXNUM,
    GARLIC_TYPE_DOUBLE,
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

/* Convert a value of type garlic_value_t into a double. */
double garlicval_to_double(garlic_value_t val);

/* Convert a double into a value of type garlic_value_t. */
garlic_value_t double_to_garlicval(double flt);

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

/* Wrap a string into a value of type garlic_value_t. */
garlic_value_t garlic_wrap_string(const char *contents);

/* To avoid having to re-allocate a garlic_value_t each time, the wrapped
 * empty string is always available as a constant. */
garlic_value_t garlic_empty_string;

/* Given a value of type garlic_value_t, get back the C string contained
 * within. */
const char * garlic_unwrap_string(garlic_value_t wrapped);

/* given a value of type garlic_value_t, representing a garlic string, get its
 * length. This function is provided because the length is cached in the value,
 * making it efficient to look up. */
size_t garlic_string_length(garlic_value_t string);

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

/* Calls the provided lambda with the given arguments. The return value of the
 * function call is then returned to the caller. */
garlic_value_t garlic_call_function(
        garlic_value_t lambda,
        garlic_value_t *args,
        size_t num_args);

#endif
