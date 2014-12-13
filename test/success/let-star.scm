(define a 4)
(define c 5)

(display
  (let* ((a 1)
         (b 2)
         (c (+ a b)))
    c)) (newline)

(display a) (newline)
(display c) (newline)

; Again, we hoist definitions inside the let* body, as well as multiple
; statements.

(let* ((hello "hello"))
       (display hello)
       (display " ")
       (display world)
       (newline)

       (define world "world"))
