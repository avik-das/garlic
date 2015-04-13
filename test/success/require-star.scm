(require "../aux/many-exports-1" *)
(require "../aux/many-exports-2" *)
(require display-helpers => dh)

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
