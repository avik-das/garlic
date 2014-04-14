(define not
        (lambda (val) (if val #f #t)))

(define cons
        (lambda (x y)
                (lambda (f) (f x y)) ))

(define car
        (lambda (pair)
                (pair (lambda (x y) x)) ))

(define cdr
        (lambda (pair)
                (pair (lambda (x y) y)) ))

(define map
        (lambda (f ls) (if ls
                           (cons (f (car ls)) (map f (cdr ls)))
                           (quote ())) ))

(define length
        (lambda (ls) (if ls
                         (+ 1 (length (cdr ls)))
                         0)) )
