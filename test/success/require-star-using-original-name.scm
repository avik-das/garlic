(require "../aux/auxillary-module" *)

(define (auxillary-function x)
  (display "local auxillary-function: ")
  (display x)
  (newline))

(auxillary-function "hello")
(auxillary-module:auxillary-function "hello")
