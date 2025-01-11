#include <garlic.h>

/* Given a list of strings, concatenate them all together. */
garlic_value_t garlic_internal_string_concat(garlic_value_t args);

/* See documentation in runtime.c */
garlic_value_t garlic_internal_append_in_place(
    garlic_value_t l1,
    garlic_value_t l2,
    garlic_value_t others);
