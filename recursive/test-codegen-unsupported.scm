; This file contains a bunch of constructs that are not supported yet by the
; codegen. As more support is added, more will be removed from this file and
; moved into whatever positive test case file is present.

102

(+ 1 2) ; Function calls are not supported
(* 2 3)
(define (a) 2) ; Lambdas and definitions are not supported
(define b 2)
(lambda (a) 2)

"abc" ; Various atoms are not supported
'atom
#f
'#f
'(1 2 3) ; List literals are not supported

(if #t 1 2) ; Conditionals are not supported
(cond
  (#t 1)
  (#f 2)
  (else 3))


205
