;; An auxillary module used by the tests. This module is not meant to be tested
;; in isolation, but it is provided as something that can be required as
;; necessary by the tests.

(display "loading auxillary-module...")
(newline)
(newline)

(define (auxillary-function x)
  (display "auxillary-function: ")
  (display x)
  (newline))

(module-export
  auxillary-function)
