(require "auxillary-module")
(require "circ1")

(display "loading circ2...") (newline)

(define (no-deps) 2)
(define (depends-on-circ1)
  (+ 10 (circ1:no-deps)))

(module-export
  no-deps
  depends-on-circ1)
