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

(define (foreach f ls)
        (if (null? ls)
            '()
            ((lambda ()
                    (f (car ls))
                    (foreach f (cdr ls))
                    '()) )) ) ; return nil no matter what

(define (identity x) x)

; varargs are necessary for implementing a proper compose function. Either this
; function has to accept varargs, or there needs to be a way to easily
; construct a list of functions (quoting doesn't work since everything inside
; the quoted list becomes a symbol, and cons isn't that convenient).
(define (compose f g)
  (lambda (x) (f (g x))) )

(display "[stdlib-test] ")
(display (length '(1 2 3))) (newline)

(module-export
  not
  length
  map
  filter
  reject
  foreach
  identity
  compose)
