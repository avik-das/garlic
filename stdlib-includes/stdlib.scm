(define (newline)
  (display "\n"))

(define (not val)
        (if val #f #t))

(define (length ls)
        (if (null? ls)
            0
            (+ 1 (length (cdr ls))) ) )

(define (append l1 l2 . others)
  ; 1. others is empty
  ; 2. others has one list
  ; 3. others has more than one list

  (define (append-one l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (append (cdr l1) l2)) ) )

  (define (helper lists)
    (if (null? lists)
        '()
        (append-one (car lists) (helper (cdr lists))) ) )

  (append-one l1 (append-one l2 (helper others))) )


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

(define (find predicate ls)
  (cond ((null? ls) #f)
        ((predicate (car ls)) (car ls))
        (else (find predicate (cdr ls))) ))

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

(define (any? pred ls)
  (cond ((null? ls) #f)
        ((pred (car ls)) #t)
        (else (any? pred (cdr ls))) ))

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
  append
  map
  filter
  reject
  find
  reduce
  sum
  foreach
  any?
  identity
  compose
  list)
