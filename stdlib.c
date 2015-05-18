#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "hashmap.h"

#define NIL_VALUE   ((garlic_value_t) 0)
#define TRUE_VALUE  ((garlic_value_t) 2)
#define FALSE_VALUE ((garlic_value_t) 4)

typedef void * garlic_value_t;

/** ENVIRONMENT FRAMES ********************************************************/

struct frame_t {
    map_t vars;
    struct frame_t *parent;
};

garlic_value_t make_fn(struct frame_t *parent_frame, void *fnpointer,
        int is_native);

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

void add_native_function_to_frame(
        struct frame_t *frame,
        char *str,
        void *fnpointer) {
    //printf("adding native fn \"%s\" w/ value %p to frame %p!\n",
    //        str, fnpointer, frame);

    hashmap_put(frame->vars, str, make_fn(frame, fnpointer, 1));
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

/** STANDARD TYPES ************************************************************/

enum garlic_value_type {
    // These types are not used except when returning the type of a value.
    GARLIC_TYPE_NIL,
    GARLIC_TYPE_BOOLEAN,
    GARLIC_TYPE_FIXNUM,

    // These types are stored alongside the values of that type.
    GARLIC_TYPE_LAMBDA,
    GARLIC_TYPE_ATOM,
    GARLIC_TYPE_STRING,
    GARLIC_TYPE_CONS,
    GARLIC_TYPE_WRAPPED_NATIVE
};

struct garlic_value {
    enum garlic_value_type type;
};

struct garlic_lambda {
    struct garlic_value super;
    struct frame_t *parent_frame;
    void *function;
    int64_t is_native;
};

struct garlic_atom {
    struct garlic_value super;
    char* name;
};

struct garlic_string {
    struct garlic_value super;
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

static inline int value_is_nil(garlic_value_t value) {
    return value == NIL_VALUE;
}

static inline int value_is_true(garlic_value_t value) {
    return value == TRUE_VALUE;
}

static inline int value_is_false(garlic_value_t value) {
    return value == FALSE_VALUE;
}

static inline int value_is_fixnum(garlic_value_t value) {
    return ((int64_t) value) & 0x1;
}

static inline int value_is_primitive(garlic_value_t value) {
    return value_is_nil(value) ||
        value_is_fixnum(value) ||
        value_is_true(value) ||
        value_is_false(value);
}

static inline int value_is_string(garlic_value_t value) {
    if (value_is_primitive(value)) {
        return 0;
    }

    return ((struct garlic_value *) value)->type == GARLIC_TYPE_STRING;
}

static inline int value_is_lambda(garlic_value_t value) {
    if (value_is_primitive(value)) {
        return 0;
    }

    return ((struct garlic_value *) value)->type == GARLIC_TYPE_LAMBDA;
}

static inline int value_is_atom(garlic_value_t value) {
    if (value_is_primitive(value)) {
        return 0;
    }

    return ((struct garlic_value *) value)->type == GARLIC_TYPE_ATOM;
}

static inline int value_is_cons(garlic_value_t value) {
    if (value_is_primitive(value)) {
        return 0;
    }

    return ((struct garlic_value *) value)->type == GARLIC_TYPE_CONS;
}

static inline int value_is_wrapped_native(garlic_value_t value) {
    if (value_is_primitive(value)) {
        return 0;
    }

    return ((struct garlic_value *) value)->type == GARLIC_TYPE_WRAPPED_NATIVE;
}

garlic_value_t make_fn(struct frame_t *parent_frame, void *fnpointer,
        int is_native) {
    struct garlic_lambda *fn = (struct garlic_lambda *)
        malloc(sizeof(struct garlic_lambda));

    fn->super.type = GARLIC_TYPE_LAMBDA;
    fn->parent_frame = parent_frame;
    fn->function = fnpointer;
    fn->is_native = is_native;

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

garlic_value_t make_string_with_contents(const char *contents) {
    struct garlic_string *string = (struct garlic_string *)
        malloc(sizeof(struct garlic_string));

    string->super.type = GARLIC_TYPE_STRING;
    string->contents = contents;

    return string;
}

/** QUOTED ATOMS **************************************************************/

map_t atom_db;

void init_atom_db() {
    if (atom_db == NULL) {
        atom_db = hashmap_new();
    }
}

void create_atom(char *name) {
    garlic_value_t atom = make_atom_from_name(name);
    hashmap_put(atom_db, name, atom);
}

garlic_value_t get_atom(char *name) {
    garlic_value_t value_ptr = (garlic_value_t) malloc(sizeof(int64_t));
    int get_result = hashmap_get(atom_db, name, &value_ptr);

    return value_ptr;
}

/** USERLAND FUNCTIONS ********************************************************/

garlic_value_t garlic_wrap_native(void *native_val) {
    struct garlic_wrapped_native *wrapped = (struct garlic_wrapped_native *)
        malloc(sizeof(struct garlic_wrapped_native));

    wrapped->super.type = GARLIC_TYPE_WRAPPED_NATIVE;
    wrapped->native_val = native_val;

    return wrapped;
}

const char * garlic_unwrap_string(garlic_value_t wrapped) {
    if (!value_is_string(wrapped)) {
        return NULL;
    }

    return ((struct garlic_string *) wrapped)->contents;
}

void * garlic_unwrap_native(garlic_value_t wrapped) {
    if (!value_is_wrapped_native(wrapped)) {
        return NULL;
    }

    return ((struct garlic_wrapped_native *) wrapped)->native_val;
}

const char * garlic_atom_name(garlic_value_t atom) {
    if (!value_is_atom(atom)) {
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
    if (!value_is_cons(cons_val)) {
        return NULL;
    }

    return ((struct garlic_cons *) cons_val)->car;
}

garlic_value_t garlic_cdr(garlic_value_t cons_val) {
    if (!value_is_cons(cons_val)) {
        return NULL;
    }

    return ((struct garlic_cons *) cons_val)->cdr;
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
