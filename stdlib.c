#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "hashmap.h"

#define NIL_VALUE   0
#define TRUE_VALUE  2
#define FALSE_VALUE 4

typedef void * scm_value_t;

scm_value_t stdlib_sum(scm_value_t vals);
scm_value_t stdlib_difference(scm_value_t vals);
scm_value_t stdlib_mul(scm_value_t vals);

scm_value_t stdlib_cons(scm_value_t car_val, scm_value_t cdr_val);
scm_value_t stdlib_car(scm_value_t cons_val);
scm_value_t stdlib_cdr(scm_value_t cons_val);
scm_value_t stdlib_nullp(scm_value_t value);
scm_value_t stdlib_equal_sign(scm_value_t vals);

scm_value_t stdlib_display(scm_value_t value);
scm_value_t stdlib_newline();

scm_value_t stdlib_impl_sum(scm_value_t vals);
scm_value_t stdlib_impl_difference(scm_value_t vals);
scm_value_t stdlib_impl_mul(scm_value_t vals);
scm_value_t stdlib_impl_nullp(scm_value_t value);
scm_value_t stdlib_impl_equal_sign(scm_value_t vals);
scm_value_t stdlib_impl_display(scm_value_t value);
scm_value_t stdlib_impl_newline();

int64_t value_to_native_int(scm_value_t value);

/** ENVIRONMENT FRAMES ********************************************************/

struct frame_t {
    map_t vars;
    struct frame_t *parent;
};

scm_value_t make_fn(struct frame_t *parent_frame, void *fnpointer,
        int is_native);

scm_value_t find_in_frame(struct frame_t *frame, char *str) {
    //printf("looking up var \"%s\" in frame %p!\n", str, frame);
    scm_value_t *value_ptr = (scm_value_t *) malloc(sizeof(int64_t));
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

void add_to_frame(struct frame_t *frame, char *str, scm_value_t value) {
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
    struct frame_t *frame = new_frame();

    add_to_frame(frame, "+", make_fn(frame, stdlib_sum, 0));
    add_to_frame(frame, "-", make_fn(frame, stdlib_difference, 0));
    add_to_frame(frame, "*", make_fn(frame, stdlib_mul, 0));

    add_to_frame(frame, "cons", make_fn(frame, stdlib_cons, 0));
    add_to_frame(frame, "car", make_fn(frame, stdlib_car, 0));
    add_to_frame(frame, "cdr", make_fn(frame, stdlib_cdr, 0));
    add_to_frame(frame, "null?", make_fn(frame, stdlib_nullp, 0));
    add_to_frame(frame, "=", make_fn(frame, stdlib_equal_sign, 0));

    add_to_frame(frame, "display", make_fn(frame, stdlib_display, 0));
    add_to_frame(frame, "newline", make_fn(frame, stdlib_newline, 0));

    return frame;
}

struct frame_t * new_frame_with_parent(struct frame_t *parent) {
    struct frame_t *frame = new_frame();
    frame->parent = parent;

    //printf("new frame %p w/ parent %p\n", frame, parent);
    return frame;
}

/** STANDARD TYPES ************************************************************/

enum scm_value_type {
    SCM_TYPE_LAMBDA = 0,
    SCM_TYPE_ATOM,
    SCM_TYPE_STRING,
    SCM_TYPE_CONS
};

struct scm_value {
    enum scm_value_type type;
};

struct scm_lambda {
    struct scm_value super;
    struct frame_t *parent_frame;
    void *function;
    int64_t is_native;
};

struct scm_atom {
    struct scm_value super;
    char* name;
};

struct scm_string {
    struct scm_value super;
    const char* contents;
};

struct scm_cons {
    struct scm_value super;
    scm_value_t car;
    scm_value_t cdr;
};

static inline int value_is_nil(scm_value_t value) {
    return value == NIL_VALUE;
}

static inline int value_is_true(scm_value_t value) {
    return ((int64_t) value) == TRUE_VALUE;
}

static inline int value_is_false(scm_value_t value) {
    return ((int64_t) value) == FALSE_VALUE;
}

static inline int value_is_fixnum(scm_value_t value) {
    return ((int64_t) value) & 0x1;
}

static inline int value_is_primitive(scm_value_t value) {
    return value_is_nil(value) ||
        value_is_fixnum(value) ||
        value_is_true(value) ||
        value_is_false(value);
}

static inline int value_is_string(scm_value_t value) {
    if (value_is_primitive(value)) {
        return 0;
    }

    return ((struct scm_value *) value)->type == SCM_TYPE_STRING;
}

static inline int value_is_lambda(scm_value_t value) {
    if (value_is_primitive(value)) {
        return 0;
    }

    return ((struct scm_value *) value)->type == SCM_TYPE_LAMBDA;
}

static inline int value_is_atom(scm_value_t value) {
    if (value_is_primitive(value)) {
        return 0;
    }

    return ((struct scm_value *) value)->type == SCM_TYPE_ATOM;
}

static inline int value_is_cons(scm_value_t value) {
    if (value_is_primitive(value)) {
        return 0;
    }

    return ((struct scm_value *) value)->type == SCM_TYPE_CONS;
}

scm_value_t make_fn(struct frame_t *parent_frame, void *fnpointer,
        int is_native) {
    struct scm_lambda *fn = (struct scm_lambda *)
        malloc(sizeof(struct scm_lambda));

    fn->super.type = SCM_TYPE_LAMBDA;
    fn->parent_frame = parent_frame;
    fn->function = fnpointer;
    fn->is_native = is_native;

    //printf("returning wrapped lambda %p pointing to %p and parent frame %p\n",
    //        fn, fnpointer, parent_frame);

    return fn;
}

scm_value_t make_atom_from_name(char *name) {
    struct scm_atom *atom = (struct scm_atom *)
        malloc(sizeof(struct scm_atom));

    atom->super.type = SCM_TYPE_ATOM;
    atom->name = name;

    return atom;
}

scm_value_t make_string_with_contents(const char *contents) {
    struct scm_string *string = (struct scm_string *)
        malloc(sizeof(struct scm_string));

    string->super.type = SCM_TYPE_STRING;
    string->contents = contents;

    return string;
}

scm_value_t make_cons(scm_value_t car_val, scm_value_t cdr_val) {
    struct scm_cons *cons = (struct scm_cons *)
        malloc(sizeof(struct scm_cons));

    cons->super.type = SCM_TYPE_CONS;
    cons->car = car_val;
    cons->cdr = cdr_val;

    //printf("returning cons %p (%p . %p)\n", cons, car_val, cdr_val);

    return cons;
}

/** QUOTED ATOMS **************************************************************/

map_t atom_db;

void init_atom_db() {
    if (atom_db == NULL) {
        atom_db = hashmap_new();
    }
}

void create_atom(char *name) {
    scm_value_t atom = make_atom_from_name(name);
    hashmap_put(atom_db, name, atom);
}

scm_value_t get_atom(char *name) {
    scm_value_t value_ptr = (scm_value_t) malloc(sizeof(int64_t));
    int get_result = hashmap_get(atom_db, name, &value_ptr);

    return value_ptr;
}

/** STANDARD FUNCTIONS ********************************************************/

scm_value_t stdlib_impl_sum(scm_value_t vals) {
    int64_t sum = 0;

    struct scm_cons *valslist = (struct scm_cons *) vals;

    while (valslist != NIL_VALUE) {
        scm_value_t val = valslist->car;
        sum += ((int64_t) val) ^ 1;
        valslist = valslist->cdr;
    }

    return (scm_value_t) (sum | 1);
}

scm_value_t stdlib_impl_difference(scm_value_t vals) {
    struct scm_cons *valslist = (struct scm_cons *) vals;

    if (valslist == NIL_VALUE) {
        printf("ERROR: - called with too few arguments");
        exit(1);
    }

    // If only one argument is passed in, then return that number negated.

    if (valslist->cdr == NIL_VALUE) {
        int64_t raw_num = ((int64_t) valslist->car) ^ 1;
        int64_t neg_num = 0 - raw_num;
        return (scm_value_t) (neg_num | 1);
    }

    // Otherwise, return the first number minus the sum of the rest of the
    // numbers.

    int64_t difference = ((int64_t) valslist->car) ^ 1;
    valslist = valslist->cdr;

    while (valslist != NIL_VALUE) {
        scm_value_t val = valslist->car;
        difference -= ((int64_t) val) ^ 1;
        valslist = valslist->cdr;
    }

    return (scm_value_t) (difference | 1);
}

scm_value_t stdlib_impl_mul(scm_value_t vals) {
    int64_t prod = 1;

    struct scm_cons *valslist = (struct scm_cons *) vals;

    while (valslist != NIL_VALUE) {
        scm_value_t val = valslist->car;
        prod *= value_to_native_int(val);
        valslist = valslist->cdr;
    }

    return (scm_value_t) ((prod << 1) | 1);
}

scm_value_t stdlib_impl_nullp(scm_value_t value) {
    return value == NIL_VALUE ?
        (scm_value_t) TRUE_VALUE :
        (scm_value_t) FALSE_VALUE;
}

scm_value_t stdlib_impl_equal_sign(scm_value_t vals) {
    struct scm_cons *valslist = (struct scm_cons *) vals;
    struct scm_cons *item;

    // There must be at least two items to check.
    if (valslist == NIL_VALUE ||
            valslist->cdr == NIL_VALUE) {
        printf("ERROR: = called with too few arguments; at least two expected");
        exit(1);
    }

    // All the items must be numbers.
    item = valslist;
    while (item != NIL_VALUE) {
        if (!value_is_fixnum(item->car)) {
            printf("ERROR: = can only compare numbers");
            exit(1);
        }

        item = item->cdr;
    }

    // All the items must be equal to the first item.
    int64_t first_value = (int64_t) valslist->car;
    item = valslist->cdr;
    while (item != NIL_VALUE) {
        int64_t item_value = (int64_t) item->car;
        if (item_value != first_value) {
            return (scm_value_t) FALSE_VALUE;
        }

        item = item->cdr;
    }

    return (scm_value_t) TRUE_VALUE;
}

void display_cons_inner(struct scm_cons *cons) {
    stdlib_impl_display(cons->car);

    if (value_is_nil(cons->cdr)) {
        // Do nothing
    } else if (value_is_cons(cons->cdr)) {
        printf(" ");
        display_cons_inner((struct scm_cons *) cons->cdr);
    } else {
        printf(" . ");
        stdlib_impl_display(cons->cdr);
    }
}

// TODO: varargs
scm_value_t stdlib_impl_display(scm_value_t value) {
    if (value_is_nil(value)) {
        printf("()");
    } else if (value_is_true(value)) {
        printf("#t");
    } else if (value_is_false(value)) {
        printf("#f");
    } else if (value_is_fixnum(value)) {
        printf("%" PRId64, value_to_native_int(value));
    } else if (value_is_string(value)) {
        printf("%s", ((struct scm_string *) value)->contents);
    } else if (value_is_lambda(value)) {
        printf("#<fn: %p>", value);
    } else if (value_is_atom(value)) {
        printf("%s", ((struct scm_atom *) value)->name);
    } else if (value_is_cons(value)) {
        struct scm_cons *cons = (struct scm_cons *) value;

        printf("(");
        display_cons_inner(cons);
        printf(")");
    }

    return NIL_VALUE;
}

scm_value_t stdlib_impl_newline() {
    printf("\n");
    return NIL_VALUE;
}
