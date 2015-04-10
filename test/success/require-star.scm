(require "../aux/many-exports-1" *)
(require "../aux/many-exports-2" *)
(require display-helpers => dh)

; It's a current known limitation that symbols imported using the starred
; import syntax will override any local symbols of the same name. This test
; codifies that limitation so that it's clear that it's fixed when the
; appropriate changes are made to the compiler.
(define (f2) "local-f2")
(define (f3) "local-f3")

(newline)

(display (f0)) (newline)
(display (f1)) (newline)
(display (f2)) (newline)
(display (f3)) (newline)

(newline)

(display (g0)) (newline)
(display (g1)) (newline)
(display (g2)) (newline)

(newline)

(dh:display-with-tag "INFO" "hello")
