(require sdl2)

(define *title* "sdl-from-scheme")
(define *width*  256)
(define *height* 256)

(define ctx (ccall sdl2:init *title* *width* *height*))
(define img (ccall sdl2:load-img ctx "bob-marley.jpg"))
(ccall sdl2:show-img ctx img)
(ccall sdl2:free-img img)

(ccall sdl2:main-loop ctx)
