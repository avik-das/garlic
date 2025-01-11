(define l1 '(1 2 3))
(define l2 '(4 5 6))
(define l3 '(7 8 9))
(define l4 '(a b c))
(define l5 '(d e f))

(display l1) (newline)
(display l2) (newline)
(display l3) (newline)
(display l4) (newline)
(display l5) (newline)

(newline)

(append-in-place l1 l2 l3 l4 l5)

(display l1) (newline)
(display l2) (newline)
(display l3) (newline)
(display l4) (newline)
(display l5) (newline)

(newline)

(display (append-in-place '(1 2 3) '() '(4 5 6))) (newline)
(display (append-in-place '() '() '(1 2 3))) (newline)
(display (append-in-place '() '() '())) (newline)
