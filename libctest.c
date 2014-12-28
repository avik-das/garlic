#include <stdio.h>

// TODO: move this to a standard header
typedef struct scm_native_export {
    char *name;
    void *fn;
} scm_native_export_t;

// TODO: move this to a standard header
#define INT_TO_FIXNUM(n) (((n) << 1) | 1)

int return_scm_num() {
    printf("Returning a number from C!\n");
    return INT_TO_FIXNUM(7);
}

scm_native_export_t libctest_exports[] = {
    {"return_scm_num", return_scm_num},
    0
};
