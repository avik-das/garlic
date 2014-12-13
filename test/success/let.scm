(define a 4)

(display
  (let ((a 1)
        (b 2))
    (+ a b))) (newline)

(display a) (newline)

; Again, we hoist definitions inside the let body, as well as multiple
; statements.

(let ((hello "hello"))
      (display hello)
      (display " ")
      (display world)
      (newline)

      (define world "world"))
