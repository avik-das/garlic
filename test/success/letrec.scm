(define a 3)
(define b 4)

(display
  (letrec ((a (lambda (x) (+ x b)))
           (b 2))
    (a 3))) (newline)

(display a) (newline)
(display b) (newline)

; The current binding should be available in its own definition in order to
; allow for recursion.
(display
  (letrec ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))) )) ))
    (fac 5)) ) (newline)

; Again, we hoist definitions inside the let* body, as well as multiple
; statements.

(letrec ((hello "hello"))
        (display hello)
        (display " ")
        (display world)
        (newline)

        (define world "world"))
