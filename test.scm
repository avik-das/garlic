(define addthreenums
  (lambda (a b c)
    (+ a (+ b c))) )

(define addfournums
  (lambda (a b c d)
    (+ ((lambda (b a) (+ b a)) a b)
       ((lambda (d c) (+ d c)) c d)) ))

(define closure
  (lambda (a)
    (lambda (b) (+ a b)) ))


(define a 3)
(define b 2)
(define c 1)

(display (addthreenums c b a)) (newline)
(display (+ a 9)) (newline)

(display (addfournums 1 2 3 4)) (newline)
(display ((closure 1) 2) )
