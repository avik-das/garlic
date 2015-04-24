(require sdl2)

(define *title* "sdl-from-garlic")
(define *width*  256)
(define *height* 256)

(define ctx (sdl2:init *title* *width* *height*))
(define img (sdl2:load-img ctx "bob-marley.jpg"))
(sdl2:show-img ctx img)
(sdl2:free-img img)

(sdl2:main-loop ctx)
