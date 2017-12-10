; While it would be possible to create a system where test programs are called
; with certain command line arguments, that's a lot of complexity for little
; benefit. Instead, just check that the `*argv*` variable exists.
;
; The name of the executable is constant, so we can rely on that.

(define (shadow-args)
  (let ((*argv* 'argv))
    (display *argv*) (newline) ))

(display *argv*) (newline)
(shadow-args)
(display *argv*) (newline)
