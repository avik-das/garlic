#include <garlic.h>

garlic_value_t nullp(garlic_value_t value) {
    return value == NIL_VALUE ?
        (garlic_value_t) TRUE_VALUE :
        (garlic_value_t) FALSE_VALUE;
}

// Many of the functions are lifted straight from the runtime, and so, they do
// not need to be re-implemented here.
garlic_native_export_t core_exports[] = {
    {"null?", nullp, 1},
    {"cons", garlic_make_cons, 2},
    {"car", garlic_car, 1},
    {"cdr", garlic_cdr, 1},
    0
};
