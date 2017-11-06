(define (newline)
  (display "\n"))

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

(define (reduce f zero ls)
        (if (null? ls)
            zero
            (reduce f (f zero (car ls)) (cdr ls)) ) )

(define (sum ls)
        (reduce + 0 ls))

(define (foreach f ls)
        (if (null? ls)
            '()
            ((lambda ()
                    (f (car ls))
                    (foreach f (cdr ls))
                    '()) )) ) ; return nil no matter what

(define (identity x) x)

(define (compose . fs)
  (define (compose2 f g)
    (lambda (x) (f (g x))) )

  (define (helper fs)
    (if (null? fs)
      identity
      (compose2 (car fs) (helper (cdr fs))) ))

  (helper fs) )

(define (list . x) x)

(module-export
  newline
  not
  length
  map
  filter
  reject
  reduce
  sum
  foreach
  identity
  compose
  list)
