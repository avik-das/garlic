#include <scm.h>
#include <SDL2/SDL.h>

#ifdef __APPLE__
  #include <SDL2_image/SDL_image.h>
#else
  #include <SDL2/SDL_image.h>
#endif

// Support for SDL 2.0.0, which doesn't include this flag. If the flag is not
// available, assume that high DPI support is also unavailable.
#ifndef SDL_WINDOW_ALLOW_HIGHDPI
  #define SDL_WINDOW_ALLOW_HIGHDPI 0
#endif

struct context {
    SDL_Window *window;
    SDL_Renderer *renderer;
    SDL_Texture *texture;

    int w;
    int h;
};

typedef struct context *context;

scm_value_t cleanup(scm_value_t ctx);

scm_value_t init(
        scm_value_t s_title,
        scm_value_t s_w,
        scm_value_t s_h) {
    context ctx = (context) malloc(sizeof(struct context));

    if (ctx == NULL) {
        printf("Unable to allocate memory for sdl_context");
        return NIL_VALUE;
    }

    if (SDL_Init(SDL_INIT_VIDEO) != 0) {
        printf("SDL_Init ERROR: %s\n", SDL_GetError());
        cleanup(ctx);
        return NIL_VALUE;
    }

    const char *title = scm_unwrap_string(s_title);
    int w = SCMVALUE_TO_INT(s_w);
    int h = SCMVALUE_TO_INT(s_h);

    ctx->window = SDL_CreateWindow(
            title,
            SDL_WINDOWPOS_UNDEFINED,
            SDL_WINDOWPOS_UNDEFINED,
            w,
            h,
            SDL_WINDOW_SHOWN | SDL_WINDOW_ALLOW_HIGHDPI
            );

    if (ctx->window == NULL) {
        printf("SDL_CreateWindow ERROR: %s\n", SDL_GetError());
        cleanup(ctx);
        return NIL_VALUE;
    }

    ctx->renderer = SDL_CreateRenderer(
            ctx->window,
            -1,
            SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC
            );

    if (ctx->renderer == NULL) {
        printf("SDL_CreateRenderer ERROR: %s\n", SDL_GetError());
        cleanup(ctx);
        return NIL_VALUE;
    }

    return scm_wrap_native(ctx);
}

scm_value_t load_img(scm_value_t s_ctx, scm_value_t s_filename) {
    const char *filename = scm_unwrap_string(s_filename);

    SDL_Surface *img = IMG_Load(filename);

    if (img == NULL) {
        printf("SDL_LoadIMG ERROR: %s\n", SDL_GetError());
        return NIL_VALUE;
    }

    return scm_wrap_native(img);
}

scm_value_t show_img(scm_value_t s_ctx, scm_value_t s_img) {
    context ctx = (context) scm_unwrap_native(s_ctx);
    SDL_Surface *img = (SDL_Surface *) scm_unwrap_native(s_img);

    ctx->texture = SDL_CreateTextureFromSurface(ctx->renderer, img);

    if (ctx->texture == NULL) {
        printf("SDL_CreateTexture ERROR: %s\n", SDL_GetError());
        cleanup(ctx);
        return NIL_VALUE;
    }

    SDL_RenderClear(ctx->renderer);
    SDL_RenderCopy(ctx->renderer, ctx->texture, NULL, NULL);
    SDL_RenderPresent(ctx->renderer);

    return NIL_VALUE;
}

scm_value_t free_image(scm_value_t s_img) {
    SDL_Surface *img = (SDL_Surface *) scm_unwrap_native(s_img);

    if (img != NULL) SDL_FreeSurface(img);
    return NIL_VALUE;
}

scm_value_t main_loop(scm_value_t s_ctx) {
    context ctx = (context) scm_unwrap_native(s_ctx);

    SDL_RenderClear(ctx->renderer);
    SDL_RenderCopy(ctx->renderer, ctx->texture, NULL, NULL);
    SDL_RenderPresent(ctx->renderer);

    int quit = 0;
    SDL_Event event;
    while (!quit) {
        SDL_WaitEvent(&event);

        switch (event.type) {
        case SDL_QUIT:
            quit = 1;
            break;
        }
    }

    cleanup(ctx);

    SDL_Quit();
    return NIL_VALUE;
}

scm_value_t cleanup(scm_value_t s_ctx) {
    context ctx = (context) scm_unwrap_native(s_ctx);

    if (ctx == NULL) return NIL_VALUE;

    if (ctx->window != NULL) SDL_DestroyWindow(ctx->window);
    if (ctx->renderer != NULL) SDL_DestroyRenderer(ctx->renderer);
    if (ctx->texture != NULL) SDL_DestroyTexture(ctx->texture);

    free(ctx);
    return NIL_VALUE;
}

scm_native_export_t sdl2_exports[] = {
    {"init", init},
    {"load-img", load_img},
    {"show-img", show_img},
    {"free-img", free_image},
    {"main-loop", main_loop},
    {"cleanup", cleanup},
    0
};
