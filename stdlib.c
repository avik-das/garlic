#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "hashmap.h"

typedef void * scm_value_t;

scm_value_t stdlib_sum(scm_value_t a_val, scm_value_t b_val);
scm_value_t stdlib_display(scm_value_t value);
scm_value_t stdlib_newline();

/** ENVIRONMENT FRAMES ********************************************************/

struct frame_t {
    map_t vars;
    struct frame_t *parent;
};

scm_value_t make_fn(struct frame_t *parent_frame, void *fnpointer);

scm_value_t find_in_frame(struct frame_t *frame, char *str) {
    //printf("looking up var \"%s\" in frame %p!\n", str, frame);
    scm_value_t *value_ptr = (scm_value_t *) malloc(sizeof(int64_t));
    int get_result = hashmap_get(frame->vars, str, value_ptr);

    if (get_result == MAP_MISSING && frame->parent) {
        *value_ptr = find_in_frame(frame->parent, str);
    }

    //printf("looking up var \"%s\" in frame %p! Got %p!\n", str, frame,
    //        *value_ptr);

    return *value_ptr;
}

void add_to_frame(struct frame_t *frame, char *str, scm_value_t value) {
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
    struct frame_t *frame = new_frame();

    add_to_frame(frame, "+", make_fn(frame, stdlib_sum));
    add_to_frame(frame, "display", make_fn(frame, stdlib_display));
    add_to_frame(frame, "newline", make_fn(frame, stdlib_newline));

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
    SCM_TYPE_LAMBDA = 0
};

struct scm_value {
    enum scm_value_type type;
};

struct scm_lambda {
    struct scm_value super;
    struct frame_t *parent_frame;
    void *function;
};

inline int value_is_fixnum(scm_value_t value) {
    return ((int64_t) value) & 0x1;
}

inline int value_is_lambda(scm_value_t value) {
    if (value_is_fixnum(value)) {
        return 0;
    }

    return ((struct scm_value *) value)->type == SCM_TYPE_LAMBDA;
}

scm_value_t make_fn(struct frame_t *parent_frame, void *fnpointer) {
    struct scm_lambda *fn = (struct scm_lambda *)
        malloc(sizeof(struct scm_lambda));

    fn->super.type = SCM_TYPE_LAMBDA;
    fn->parent_frame = parent_frame;
    fn->function = fnpointer;

    //printf("returning wrapped lambda %p pointing to %p and parent frame %p\n",
    //        fn, fnpointer, parent_frame);

    return fn;
}

/** STANDARD FUNCTIONS ********************************************************/

scm_value_t stdlib_impl_sum(scm_value_t a_val, scm_value_t b_val) {
    //printf("summing %ld + %ld\n", (int64_t) a_val, (int64_t) b_val);
    int64_t a = ((int64_t) a_val) ^ 1;
    int64_t b = ((int64_t) b_val) ^ 1;
    int64_t c = a + b;

    return (scm_value_t) (c | 1);
}

// TODO: varargs
scm_value_t stdlib_impl_display(scm_value_t value) {
    if (value_is_fixnum(value)) {
        printf("%ld", ((int64_t) value) >> 1);
    } else if (value_is_lambda(value)) {
        printf("#<fn: %p>", value);
    }

    // TODO: return nil
    return NULL;
}

scm_value_t stdlib_impl_newline() {
    printf("\n");
    return NULL; // TODO: return nil
}
