(require display-helpers)

(newline)

(define (varargs x y . z) (cons (+ x y) z))
(display (varargs 1 2 3 4 5)) (newline)

(newline)

(display (list 'a 'b 'c 'd)) (newline)
(display-helpers:display-all-with-tag "INFO"
                                      "1st message"
                                      "2nd message"
                                      "3rd message")

(newline)

; varargs work for lambdas too
(display
  ((lambda (x y . z)
     (+ (* x y)
        (sum z)) ) 2 3 4 5 6) ) (newline)

; Note that because lambdas have no names, the syntax to specify no positional
; arguments is a little different: you have to specify the entire argument list
; as a single variable, not a list.
(display ((lambda ls (sum ls)) 1 2 3)) (newline)
