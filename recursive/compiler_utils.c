/**
 * A set of utilities for the recursive compiler that are *not* general purpose
 * modules useful for the language's standard library.
 *
 * This sort of defeats the purpose of the recursive compiler, since the goal
 * is for the language to be powerful enough to implement a compiler for itself
 * without resorting to C. For that reason, this module should be as minimal as
 * possible, with the goal to replace them with in-language implementations in
 * the future.
 *
 * Each function should be documented with its rationale for being implemented
 * in this module.
 */

#include <garlic.h>

#include <stdlib.h>
#include <string.h>

/**
 * RATIONALE: this should actually end up in a "path" module.  However, I'd
 * rather design a proper "path" module, not just a one-off function that won't
 * fit into a more cohesive design.
 */
garlic_value_t filename_from_path(garlic_value_t path) {
    const char *pathstr = garlic_unwrap_string(path);
    if (!pathstr) {
        error_and_exit(
                "ERROR - filename_from_path: "
                "path is not a string\n");
    }

    int len = strlen(pathstr);
    if (len == 0) { return garlic_empty_string; }

    int i = len - 1;                            // Go to the end
    while (i > 0 && pathstr[i] != '/') { i--; } // Backtrack to the last '/'
    if (pathstr[i] == '/') { i++; }             // Go past that '/'

    int result_len = len - i;
    char *result = (char *) malloc(sizeof(char) * (result_len + 1));
    if (!result) {
        error_and_exit(
                "ERROR - filename-from-path: "
                "could not allocate memory for result\n");
    }

    strncpy(result, pathstr + i, result_len);
    result[result_len] = 0;
    return garlic_wrap_string(result);
}

/**
 * RATIONALE: this could be written in garlic, but it would depend heavily on
 * string manipulation, which is implemented inefficiently via the not
 * well-designed "string" C module. This would be better to reimplement once
 * string manipulation is thought out properly.
 */
garlic_value_t line_from_file_contents(
        garlic_value_t contents,
        garlic_value_t lineno) {
    const char *contents_str = garlic_unwrap_string(contents);
    if (!contents_str) {
        error_and_exit(
                "ERROR - line-from-file-contents: contents is not a string\n");
    }

    int64_t lineno_int = garlicval_to_int(lineno);

    int i = 0;
    int curr_line = 1;
    int line_start = 0;

    // Find start of desired line
    while (contents_str[i] && curr_line < lineno_int) {
        // TODO - support "\r\n"
        if (contents_str[i] == '\n') {
            curr_line++;
            line_start = i + 1;
        }

        i++;
    }

    // Find end of that line
    while (contents_str[i] && contents_str[i] != '\n') { i++; }

    int result_len = i - line_start;
    char *result = (char *) malloc(sizeof(char) * (result_len + 1));
    if (!result) {
        error_and_exit(
                "ERROR - filename-from-path: "
                "could not allocate memory for result\n");
    }

    strncpy(result, contents_str + line_start, result_len);
    result[result_len] = 0;
    return garlic_wrap_string(result);
}

garlic_native_export_t compiler_utils_exports[] = {
    {"filename-from-path", filename_from_path, 1},
    {"line-from-file-contents", line_from_file_contents, 2},
    0
};
