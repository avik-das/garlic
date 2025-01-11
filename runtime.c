#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <garlic.h>

#include "hashmap.h"

/** ENVIRONMENT FRAMES ********************************************************/

struct frame_t {
    map_t vars;
    struct frame_t *parent;
};

garlic_value_t make_fn(struct frame_t *parent_frame, void *fnpointer);

garlic_value_t find_in_frame(struct frame_t *frame, char *str) {
    //printf("looking up var \"%s\" in frame %p!\n", str, frame);
    garlic_value_t *value_ptr = (garlic_value_t *) malloc(sizeof(int64_t));
    int get_result = hashmap_get(frame->vars, str, value_ptr);

    if (get_result == MAP_MISSING) {
        if (frame->parent) {
            *value_ptr = find_in_frame(frame->parent, str);
        } else {
            printf("ERROR: undefined variable \"%s\"\n", str);
            exit(1);
        }
    }

    //printf("looking up var \"%s\" in frame %p! Got %p!\n", str, frame,
    //        *value_ptr);

    return *value_ptr;
}

void add_to_frame(struct frame_t *frame, char *str, garlic_value_t value) {
    //printf("adding var \"%s\" w/ value %p to frame %p!\n", str, value, frame);
    hashmap_put(frame->vars, str, value);
}

struct frame_t * new_frame() {
    struct frame_t *frame = (struct frame_t *) malloc(sizeof(struct frame_t));
    frame->vars = hashmap_new();
    frame->parent = NULL;

    return frame;
}

struct frame_t * new_root_frame() {
    // This function exists mostly as documentation; calling this function
    // signifies that the returned frame, though not any different from other
    // newly-created frames, will be used as a module's root frame.
    return new_frame();
}

struct frame_t * new_frame_with_parent(struct frame_t *parent) {
    struct frame_t *frame = new_frame();
    frame->parent = parent;

    //printf("new frame %p w/ parent %p\n", frame, parent);
    return frame;
}

/** INITIALIZATION ************************************************************/

void create_argv(struct frame_t *frame, int argc, char **argv) {
    garlic_value_t arglist = NIL_VALUE;
    for (int i = argc - 1; i >= 0; i--) {
        garlic_value_t arg = garlic_wrap_string(argv[i]);
        arglist = garlic_make_cons(arg, arglist);
    }

    add_to_frame(frame, "*argv*", arglist);
}

/** STANDARD TYPES ************************************************************/

struct garlic_value {
    enum garlic_value_type type;
};

struct garlic_double {
    struct garlic_value super;
    double value;
};

struct garlic_lambda {
    struct garlic_value super;
    struct frame_t *parent_frame;
    void *function;
};

struct garlic_atom {
    struct garlic_value super;
    char* name;
};

struct garlic_string {
    struct garlic_value super;
    size_t length;
    // NOTE: even though the length of a string is stored, the contents are
    // still stored as a null-terminated string. This allows easy unwrapping to
    // a C-style string, for use in C extensions.
    const char* contents;
};

struct garlic_cons {
    struct garlic_value super;
    garlic_value_t car;
    garlic_value_t cdr;
};

struct garlic_wrapped_native {
    struct garlic_value super;
    void *native_val;
};

garlic_value_t make_fn(struct frame_t *parent_frame, void *fnpointer) {
    struct garlic_lambda *fn = (struct garlic_lambda *)
        malloc(sizeof(struct garlic_lambda));

    fn->super.type = GARLIC_TYPE_LAMBDA;
    fn->parent_frame = parent_frame;
    fn->function = fnpointer;

    //printf("returning wrapped lambda %p pointing to %p and parent frame %p\n",
    //        fn, fnpointer, parent_frame);

    return fn;
}

garlic_value_t make_atom_from_name(char *name) {
    struct garlic_atom *atom = (struct garlic_atom *)
        malloc(sizeof(struct garlic_atom));

    atom->super.type = GARLIC_TYPE_ATOM;
    atom->name = name;

    return atom;
}

/** QUOTED ATOMS **************************************************************/

map_t atom_db;

void init_atom_db() {
    if (atom_db == NULL) {
        atom_db = hashmap_new();
    }
}

garlic_value_t create_atom(char *name) {
    garlic_value_t atom = make_atom_from_name(name);
    hashmap_put(atom_db, name, atom);

    return atom;
}

garlic_value_t get_atom(char *name) {
    garlic_value_t value_ptr = (garlic_value_t) malloc(sizeof(int64_t));
    int result = hashmap_get(atom_db, name, &value_ptr);

    if (result == MAP_MISSING) {
        free(value_ptr);
        return NIL_VALUE;
    }

    return value_ptr;
}

garlic_value_t garlic_intern_atom(char *name) {
    garlic_value_t existing = get_atom(name);
    if (existing != NIL_VALUE) {
        return existing;
    }

    return create_atom(name);
}

/** RUNTIME CHECKS ************************************************************/

void check_destructuring_assignment(garlic_value_t rvalue) {
    if (garlic_get_type(rvalue) != GARLIC_TYPE_CONS) {
        error_and_exit("unable to unpack list");
    }
}

void check_destructuring_terminated_in_nil(garlic_value_t rvalue) {
    if (rvalue != NIL_VALUE) {
        error_and_exit("destructured list did not end in NIL");
    }
}

/** USERLAND FUNCTIONS ********************************************************/

void error_and_exit(const char *message) {
    printf("\033[1;31merr: %s\033[0m\n", message);
    exit(1);
}

double garlicval_to_double(garlic_value_t wrapped) {
    if (garlic_get_type(wrapped) != GARLIC_TYPE_DOUBLE) {
        return NAN;
    }

    return ((struct garlic_double *) wrapped)->value;
}

garlic_value_t double_to_garlicval(double flt) {
    struct garlic_double *wrapped = (struct garlic_double *)
        malloc(sizeof(struct garlic_double));

    wrapped->super.type = GARLIC_TYPE_DOUBLE;
    wrapped->value = flt;

    return wrapped;
}

garlic_value_t garlic_empty_string = &(struct garlic_string) {
    {GARLIC_TYPE_STRING},
    0,
    ""
};

garlic_value_t garlic_wrap_native(void *native_val) {
    struct garlic_wrapped_native *wrapped = (struct garlic_wrapped_native *)
        malloc(sizeof(struct garlic_wrapped_native));

    wrapped->super.type = GARLIC_TYPE_WRAPPED_NATIVE;
    wrapped->native_val = native_val;

    return wrapped;
}

garlic_value_t garlic_wrap_string(const char *contents) {
    struct garlic_string *string = (struct garlic_string *)
        malloc(sizeof(struct garlic_string));

    string->super.type = GARLIC_TYPE_STRING;
    string->length = strlen(contents);
    string->contents = contents;

    return string;
}

garlic_value_t garlic_internal_string_concat(garlic_value_t args) {
    if (args == NIL_VALUE) {
        return garlic_empty_string;
    }

    garlic_value_t args_copy = args;

    size_t total_length = 0;
    while (args_copy != NIL_VALUE) {
        garlic_value_t item = garlic_car(args_copy);
        args_copy = garlic_cdr(args_copy);

        if (garlic_get_type(item) != GARLIC_TYPE_STRING) {
            printf("non-string passed to concat\n");
            return NIL_VALUE;
        }

        total_length += garlic_string_length(item);
    }

    total_length++; // for the NULL-terminator

    char *result = (char *) malloc(sizeof(char) * total_length);
    size_t start = 0;
    args_copy = args;
    while (args_copy != NIL_VALUE) {
        garlic_value_t item = garlic_car(args_copy);
        args_copy = garlic_cdr(args_copy);

        const char *str = garlic_unwrap_string(item);
        strcpy(result + start, str);
        start += garlic_string_length(item);
    }

    return garlic_wrap_string(result);
}

const char * garlic_unwrap_string(garlic_value_t wrapped) {
    if (garlic_get_type(wrapped) != GARLIC_TYPE_STRING) {
        return NULL;
    }

    return ((struct garlic_string *) wrapped)->contents;
}

void * garlic_unwrap_native(garlic_value_t wrapped) {
    if (garlic_get_type(wrapped) != GARLIC_TYPE_WRAPPED_NATIVE) {
        return NULL;
    }

    return ((struct garlic_wrapped_native *) wrapped)->native_val;
}

size_t garlic_string_length(garlic_value_t string) {
    if (garlic_get_type(string) != GARLIC_TYPE_STRING) {
        return 0;
    }

    return ((struct garlic_string *) string)->length;
}

const char * garlic_atom_name(garlic_value_t atom) {
    if (garlic_get_type(atom) != GARLIC_TYPE_ATOM) {
        return NULL;
    }

    return ((struct garlic_atom *) atom)->name;
}

garlic_value_t garlic_make_cons(garlic_value_t car_val,
        garlic_value_t cdr_val) {
    struct garlic_cons *cons = (struct garlic_cons *)
        malloc(sizeof(struct garlic_cons));

    cons->super.type = GARLIC_TYPE_CONS;
    cons->car = car_val;
    cons->cdr = cdr_val;

    //printf("returning cons %p (%p . %p)\n", cons, car_val, cdr_val);

    return cons;
}

garlic_value_t garlic_car(garlic_value_t cons_val) {
    if (garlic_get_type(cons_val) != GARLIC_TYPE_CONS) {
        return NULL;
    }

    return ((struct garlic_cons *) cons_val)->car;
}

garlic_value_t garlic_cdr(garlic_value_t cons_val) {
    if (garlic_get_type(cons_val) != GARLIC_TYPE_CONS) {
        return NULL;
    }

    return ((struct garlic_cons *) cons_val)->cdr;
}

/* Append the given lists, in the order given. Mutates all but the last list.
 * Generally, Garlic functions don't generally mutate their arguments, but this
 * function is needed for performance reasons. Be very careful not to use this
 * the original lists after call this function.
 *
 * @param l1 - the first list to append
 * @param l2 - the second list to append
 * @param others - any other lists to append, may be empty
 * @return `l1`, but now it's been mutated to include `l2` and the other lists
 *         afterwards
 */
garlic_value_t garlic_internal_append_in_place(
    garlic_value_t l1,
    garlic_value_t l2,
    garlic_value_t others) {
  if (l1 != NIL_VALUE && garlic_get_type(l1) != GARLIC_TYPE_CONS) {
    error_and_exit("append-in-place: l1 is not a list");
  }

  if (l2 != NIL_VALUE && garlic_get_type(l2) != GARLIC_TYPE_CONS) {
    error_and_exit("append-in-place: l2 is not a list");
  }

  // Assume the runtime has ensured `others` is a list.
  garlic_value_t others_for_checking = others;
  while (others_for_checking != NIL_VALUE) {
    garlic_value_t other_for_checking = garlic_car(others_for_checking);
    others_for_checking = garlic_cdr(others_for_checking);

    if (other_for_checking != NIL_VALUE &&
        garlic_get_type(other_for_checking) != GARLIC_TYPE_CONS) {
      error_and_exit("append-in-place: additional param is not a list");
    }
  }

  garlic_value_t to_return = l1;

  garlic_value_t ptr = l1;
  garlic_value_t last = NIL_VALUE;
  while (ptr != NIL_VALUE) {
    last = ptr;
    ptr = garlic_cdr(ptr);
  }

  if (to_return == NIL_VALUE) { to_return = l2; }

  if (l2 != NIL_VALUE) {
    if (last != NIL_VALUE) {
      ((struct garlic_cons *) last)->cdr = l2;
    }

    ptr = l2;
    while (ptr != NIL_VALUE) {
      last = ptr;
      ptr = garlic_cdr(ptr);
    }
  }

  garlic_value_t othersptr = others;
  while (othersptr != NIL_VALUE) {
    garlic_value_t ptr = garlic_car(othersptr);
    othersptr = garlic_cdr(othersptr);

    if (to_return == NIL_VALUE) { to_return = ptr; }

    if (ptr != NIL_VALUE) {
      if (last != NIL_VALUE) {
        ((struct garlic_cons *) last)->cdr = ptr;
      }

      while (ptr != NIL_VALUE) {
        last = ptr;
        ptr = garlic_cdr(ptr);
      }
    }
  }

  if (to_return == NIL_VALUE) {
    return NIL_VALUE;
  }

  return to_return;
}

enum garlic_value_type garlic_get_type(garlic_value_t val) {
    if (val == NIL_VALUE) {
        return GARLIC_TYPE_NIL;
    }

    if (val == TRUE_VALUE || val == FALSE_VALUE) {
        return GARLIC_TYPE_BOOLEAN;
    }

    if (((int64_t) val) & 0x1) {
        return GARLIC_TYPE_FIXNUM;
    }

    return ((struct garlic_value *) val)->type;
}
