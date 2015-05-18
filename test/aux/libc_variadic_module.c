#include <stdio.h>
#include <garlic.h>

// This function will be exported twice under two different names, once where
// the exports list specifies the is_vararg flag, and once where it doesn't.
// This will test that the flag defaults to "false".
garlic_value_t non_variadic_add(garlic_value_t a, garlic_value_t b) {
    return int_to_garlicval(garlicval_to_int(a) + garlicval_to_int(b));
}

// This is a private function used to implement the rest of the functions.
garlic_value_t add_all(garlic_value_t items) {
    int64_t sum = 0;

    while (items != NIL_VALUE) {
        garlic_value_t item = garlic_car(items);
        items = garlic_cdr(items);

        sum += garlicval_to_int(item);
    }

    return int_to_garlicval(sum);
}

garlic_value_t variadic0_add(garlic_value_t items) {
    return add_all(items);
}

garlic_value_t variadic1_add(garlic_value_t arg0, garlic_value_t items) {
    printf("Required args: %" PRId64 "\n", garlicval_to_int(arg0));
    return add_all(items);
}

garlic_value_t variadic9_add(
        garlic_value_t arg0,
        garlic_value_t arg1,
        garlic_value_t arg2,
        garlic_value_t arg3,
        garlic_value_t arg4,
        garlic_value_t arg5,
        garlic_value_t arg6,
        garlic_value_t arg7,
        garlic_value_t arg8,
        garlic_value_t items) {
    printf("Required args: %" PRId64 "\n"
           "               %" PRId64 "\n"
           "               %" PRId64 "\n"
           "               %" PRId64 "\n"
           "               %" PRId64 "\n"
           "               %" PRId64 "\n"
           "               %" PRId64 "\n"
           "               %" PRId64 "\n"
           "               %" PRId64 "\n",
           garlicval_to_int(arg0),
           garlicval_to_int(arg1),
           garlicval_to_int(arg2),
           garlicval_to_int(arg3),
           garlicval_to_int(arg4),
           garlicval_to_int(arg5),
           garlicval_to_int(arg6),
           garlicval_to_int(arg7),
           garlicval_to_int(arg8));
    return add_all(items);
}

garlic_native_export_t libc_variadic_module_exports[] = {
    {"non_variadic_add_no_flag", non_variadic_add, 2},
    {"non_variadic_add_flag", non_variadic_add, 2, 0},
    {"variadic0_add", variadic0_add, 0, 1},
    {"variadic1_add", variadic1_add, 1, 1},
    {"variadic9_add", variadic9_add, 9, 1},
    0
};
