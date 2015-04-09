(require "auxillary-module")
(require "circ2")

(display "loading circ1...") (newline)

(define (no-deps) 1)
(define (depends-on-circ2)
  (+ 10 (circ2:no-deps)))

(module-export
  no-deps
  depends-on-circ2)
