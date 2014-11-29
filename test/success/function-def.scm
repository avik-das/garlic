(define (f x)
  (+ 1 x))

(define (g a b c d)
  (cons a (cons b (cons c (cons d '())))) )

(display (f 2)) (newline)
(display (g 1 2 3 4)) (newline)
