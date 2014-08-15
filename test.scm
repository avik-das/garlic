;; Adds the three numberical inputs.
(define addthreenums
  ;; This doesn't use the procedure definition syntax
  (lambda (a b c)
    (+ a (+ b c))) )

;; Adds the four numberical inputs. This demonstrates nested lambdas, and the
;; fact that variable shadowing works. The four variables are bound in all
;; manner of ways inside the inner lambdas, but there's no conflict because
;; each lambda introduces its own scope.
(define addfournums
  (lambda (a b c d)
    (+ ((lambda (d c) (+ d c)) a b)
       ;; really complicated bindings!
       ((lambda (b a) (+ b a)) c d)) ))

;; Given a number, returns a function that takes in another number and adds the
;; two numbers together. This demonstrates that lexical scoping works, and that
;; each lambda references its enclosing scope.
(define closure
  (lambda (a)
    (lambda (b) (+ a b)) ))

;; These variable names are deliberately named in conflict with the various
;; names that are present as the lambda arguments to prove that these bindings
;; are not blown away when the lambdas are invoked.
(define a 3)
(define b 2)
(define c 1)

(display (addthreenums c b a)) (newline)
(display (+ a 9)) (newline) ; Prove that "a" is not blown away by calling
                            ; addthreenums

(display (addfournums 1 2 3 4)) (newline)
(display ((closure 1) 2) ) (newline)

(newline)

;; Just for kicks, displaying a lambda.
(display closure) (newline)

(newline)

;; Function definition syntax

(define (f x)
  ;; Now we're using the function definition syntax
  (+ 1 x))

(define (g a b c d)
  (cons a (cons b (cons c (cons d '())))) )

(display (f 2)) (newline)
(display '(this is a list)) (newline)
(display (g 'this 'is 'a 'list)) (newline)

(newline)

;; Quoted values

(display 'a) (newline)
(display '123) (newline)
(display '#t) (newline)
(display '#f) (newline)
(display '(1 2 3)) (newline)
(display '(1 '(2 3) 4)) (newline)

(newline)

;; Nil.
(display '()) (newline)
(display (display 'a)) (newline)

(newline)

;; Cons cells.
(define threecons (cons 1 (cons 2 3)))
(display threecons) (newline)
(display (car threecons)) (newline)
(display (cdr threecons)) (newline)

(display (cons 'a (cons 'b (cons 'c '())))) (newline)
(display '(a b c)) (newline)

(display (null? '(a b c))) (newline)
(display (null? '())) (newline)
(display (null? 0)) (newline)

(newline)

;; Booleans and onditionals.

(define not
        (lambda (val) (if val #f #t)))

(display #t) (newline)
(display #f) (newline)
(display (if #t 'correct 'wrong)) (newline)
(display (if #f 'wrong 'correct)) (newline)
(display (if '() 'correct 'wrong)) (newline)
(display (if 0 'correct 'wrong)) (newline)
(display (if 1 'correct 'wrong)) (newline)
(display (if (not #f) 'correct 'wrong)) (newline)
(display (if (not #t) 'wrong 'correct)) (newline)
(display (if (not 0) 'wrong 'correct)) (newline)
(display (if (not 1) 'wrong 'correct)) (newline)

(newline)

;; Lists and recursion.

(define length
        (lambda (ls) (if (null? ls)
                         0
                         (+ 1 (length (cdr ls))) )) )

(define map
        (lambda (f ls) (if (null? ls)
                           '()
                           (cons (f (car ls)) (map f (cdr ls))) )) )

(display (length '(a b c d))) (newline)
(display (map (lambda (x) (+ 1 x)) '(1 2 3 4)))
