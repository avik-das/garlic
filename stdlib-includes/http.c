#include <garlic.h>
#include <microhttpd.h>
#include <stdio.h>
#include <string.h>
#include <signal.h>

int handle_request(
        void *cls,
        struct MHD_Connection *connection,
        const char *url,
        const char *method,
        const char *version,
        const char *upload_data,
        size_t *upload_data_size,
        void **con_cls) {
    static int connection_marker;

    if (strcmp(method, "GET") != 0) {
        printf("HTTP server only accepts GET requests for now\n");
        return MHD_NO;
    }

    if (*con_cls != &connection_marker) {
        // The first time, only the headers are valid. Do not respond in the
        // first round.
        *con_cls = &connection_marker;
        return MHD_YES;
    }

    *con_cls = NULL; // clear context pointer

    garlic_value_t callback = cls;
    garlic_value_t callback_args[] = {};
    garlic_value_t response_body = garlic_call_function(
            callback,
            callback_args,
            0);

    const char *response_body_str = garlic_unwrap_string(response_body);

    struct MHD_Response *response = MHD_create_response_from_buffer(
            strlen(response_body_str),
            (void *)response_body_str,
            MHD_RESPMEM_PERSISTENT);

    int ret = MHD_queue_response(
            connection,
            MHD_HTTP_OK,
            response);

    MHD_destroy_response(response);
    return ret;
}

garlic_value_t serve(garlic_value_t port, garlic_value_t callback) {
    struct MHD_Daemon *d = MHD_start_daemon(
            MHD_USE_THREAD_PER_CONNECTION,
            garlicval_to_int(port),
            NULL,
            NULL,
            &handle_request,
            callback,
            MHD_OPTION_END);

    if (d == NULL) {
        printf("Unable to start HTTP server\n");
        return NIL_VALUE;
    }

    printf("Started HTTP server on port %" PRId64 "\n",
            garlicval_to_int(port));

    // Wait indefinitely for the user to interrupt the process.
    sigset_t mask;
    sigemptyset(&mask);
    sigsuspend(&mask);

    MHD_stop_daemon(d);

    return NIL_VALUE;
}

garlic_native_export_t http_exports[] = {
    {"serve", serve, 2},
    0
};
