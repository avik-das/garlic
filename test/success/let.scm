(define a 4)

(display
  (let ((a 1)
        (b 2))
    (+ a b))) (newline)

(display a) (newline)

; The current binding should be available in its own definition in order to
; allow for recursion.
(display
  (let ((fac (lambda (n) (if (= n 0) 1 (* n (fac (- n 1))) )) ))
    (fac 5)) ) (newline)

; Again, we hoist definitions inside the let body, as well as multiple
; statements.

(let ((hello "hello"))
      (display hello)
      (display " ")
      (display world)
      (newline)

      (define world "world"))
