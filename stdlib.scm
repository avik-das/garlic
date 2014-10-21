(display "loading stdlib...")
(newline)

(define (not val)
        (if val #f #t))

(define (length ls)
        (if (null? ls)
            0
            (+ 1 (length (cdr ls))) ) )

(define (map f ls)
        (if (null? ls)
            '()
            (cons (f (car ls)) (map f (cdr ls))) ) )

(define (filter f ls)
        (if (null? ls)
            '()
            (if (f (car ls))
                (cons (car ls) (filter f (cdr ls)))
                (filter f (cdr ls)) ) ) )

(define (reject f ls)
        (filter (lambda (x) (not (f x))) ls) )

(display "[stdlib-test] ")
(display (length '(1 2 3)))

(module-export
  length
  map
  filter
  reject)
