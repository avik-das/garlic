#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#include "hashmap.h"

/** STANDARD TYPES ************************************************************/

typedef void * scm_value_t;

/** STANDARD FUNCTIONS ********************************************************/

scm_value_t stdlib_sum(scm_value_t a_val, scm_value_t b_val) {
    int64_t a = ((int64_t) a_val) ^ 1;
    int64_t b = ((int64_t) b_val) ^ 1;
    int64_t c = a + b;

    return (scm_value_t) (c | 1);
}

// TODO: varargs
scm_value_t stdlib_display(scm_value_t value) {
    if (((int64_t) value) & 0x1) {
        // a tagged fixnum
        printf("%ld", ((int64_t) value) >> 1);
    }

    // TODO: return nil
    return NULL;
}

/** ENVIRONMENT FRAMES ********************************************************/

struct frame_t {
    map_t vars;
    struct frame_t *parent;
};

scm_value_t find_in_frame(struct frame_t * frame, char *str) {
    scm_value_t *value_ptr = (scm_value_t *) malloc(sizeof(int64_t));
    hashmap_get(frame->vars, str, value_ptr);
    //printf("looking up var \"%s\" in frame %p! Got %p!\n", str, frame,
    //        *value_ptr);

    return *value_ptr;
}

void add_to_frame(struct frame_t * frame, char *str, scm_value_t value) {
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

    add_to_frame(frame, "+", stdlib_sum);
    add_to_frame(frame, "display", stdlib_display);

    return frame;
}

struct frame_t * new_frame_with_parent(struct frame_t * parent) {
    return NULL;
    // TODO
}
