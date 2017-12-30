(display (+ (- 1 2) (* 3 (+ 4 5)) 6)) (newline)
(display (+ 7 8)) (newline)
(display 10) (newline)

(newline)

(define x 3)
(define y (+ x 4))
(display x) (newline)
(display (+ x 2)) (newline)
(display y) (newline)

(newline)

(display ((lambda (x) (+ 1 x)) 3)) (newline)
(define f (lambda (y) (* 3 y)))
(display (f 2)) (newline)
(display x) (newline)
(display y) (newline)

(newline)

(define closure
  (lambda (a)
    (lambda (b) (+ a b x)) ))
(display ((closure 1) 2) ) (newline)

(newline)

(define (f x) (+ 1 x))

(define (g a b c d)
  (cons a (cons b (cons c d))) )

(display (f 2)) (newline)
(display (g 1 2 3 4)) (newline)

(newline)
