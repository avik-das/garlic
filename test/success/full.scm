;; Note that some loading messages will be displayed first, since all required
;; modules are initialized at the very beginning.

(newline)

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

;; Numerical values

(display  2) (newline) ; a positive number
(display +2) (newline) ; a positive number with a plus sign
(display -2) (newline) ; a negative number (even)
(display -1) (newline) ; a negative number (odd)
(display (+ 3 -3)) (newline)
(display (+ 9 -3)) (newline)
(display (+ 3 -9)) (newline)
(display (- 3)) (newline)
(display (- 3 1)) (newline)
(display (- 3 4)) (newline)
(display (- 3 1 2 3)) (newline)
(display (* 2 -3)) (newline)
(display (* 2 -3 4)) (newline)
(display (* 2 -3 4 -5)) (newline)
(display (* -1 -3 -5)) (newline)

(newline)

;; Quoted values

(display 'a) (newline)
(display '123) (newline)
(display '#t) (newline)
(display '#f) (newline)
(display '(1 2 3)) (newline)
(display '(1 (2 3) 4)) (newline)

(newline)

;; Nil

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

(define my-not
  (lambda (val) (if val #f #t)))

(display #t) (newline)
(display #f) (newline)
(display (if #t 'correct 'wrong)) (newline)
(display (if #f 'wrong 'correct)) (newline)
(display (if '() 'correct 'wrong)) (newline)
(display (if 0 'correct 'wrong)) (newline)
(display (if 1 'correct 'wrong)) (newline)
(display (if (my-not #f) 'correct 'wrong)) (newline)
(display (if (my-not #t) 'wrong 'correct)) (newline)
(display (if (my-not 0) 'wrong 'correct)) (newline)
(display (if (my-not 1) 'wrong 'correct)) (newline)
(display (= 1 1)) (newline)
(display (= 2 2 (+ 1 1))) (newline)
(display (my-not (= 1 2))) (newline)
(display (my-not (= 1 1 2))) (newline)

(newline)

; Cond expressions

(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2)) )) ))

(display (fib 0)) (newline)
(display (fib 1)) (newline)
(display (fib 2)) (newline)
(display (fib 3)) (newline)
(display (fib 4)) (newline)
(display (fib 5)) (newline)
(display (fib 6)) (newline)
(display (fib 7)) (newline)
(display (fib 8)) (newline)
(display (fib 9)) (newline)
(display (fib 10)) (newline)

(newline)

(define (check-1-2-3 n)
  (cond ((= n 1) 'one)
        ((= n 2) 'two)
        ((= n 3) 'three)) )

(display (check-1-2-3 1)) (newline)
(display (check-1-2-3 2)) (newline)
(display (check-1-2-3 3)) (newline)

(newline)

;; Lists and recursion.

; Note that the definitions of length and map come *after* their usage, but
; that's fine because the functions are defined in the same scope.
(display (length '(a b c d))) (newline)
(display (map (lambda (x) (+ 1 x)) '(1 2 3 4))) (newline)

(define length
        (lambda (ls) (if (null? ls)
                         0
                         (+ 1 (length (cdr ls))) )) )

(define map
        (lambda (f ls) (if (null? ls)
                           '()
                           (cons (f (car ls)) (map f (cdr ls))) )) )

(newline)

;; Improper lists

(display '(1 . 2)) (newline)
(display '(1 2 . 3)) (newline)
(display (cdr '(1 2 . 3))) (newline)
(display (cdr (cdr '(1 2 . 3)))) (newline)

;; Strings

(newline)

(display "a string") (newline)
(display "some numbers and symbols: 1 2 3 # @ <- cool!") (newline)
(display "a string with a newline afterwards\n")
(display "\ta string with an embedded tab") (newline)
(display "a string with \"escaped\" quotes") (newline)
(display "no need to 'escape' single quotes") (newline)
(display "") (newline)
(display "^ empty string above") (newline)

;; Multiple statements in function body

(newline)

(define (print-with-newline x)
  (display "PRINT-WITH-NEWLINE: " x)
  (newline))

(print-with-newline 'hello)

((lambda (x)
   (display "LAMBDA-VERSION: " x)
   (newline)) "hello again")

;; Scoped definitions

(newline)

(define shadowed 'outer)

((lambda ()
   (display shadowed)
   (newline)
   ; Again, the definition comes *after* the usage, but the definition is
   ; hoist up to the top of the scope that defines the lambda body.
   (define shadowed 'inner)) )

(display shadowed) (newline)

;; Multiple modules

(newline)

(require display-helpers => disp)

(display (length '(a b c d))) (newline)
(display (map (lambda (x) (+ 1 x)) '(1 2 3 4))) (newline)
(display (reject null? '(1 '() 3 '()))) (newline)

(newline)

(disp:display-with-tag
  "INFO"
  "this is calling a function in display-helpers")
(disp:display-with-tag "DEBUG" "debugging output...")

(newline)

(disp:display-non-null
  '("one" () "two" () "three"))

(newline)

;; Variable number of arguments

(display (+)) (newline)
(display (+ 1)) (newline)
(display (+ 1 2)) (newline)
(display (+ 1 2 3)) (newline)
(display (+ 1 2 3 4)) (newline)
(display (+ 1 2 3 4 5)) (newline)

(display (*)) (newline)
(display (* 2)) (newline)
(display (* 2 3)) (newline)
(display (* 2 3 4)) (newline)
(display (* 2 3 4 5)) (newline)

(newline)

(define (varargs x y . z) (cons (+ x y) z))
(display (varargs 1 2 3 4 5)) (newline)

(newline)

(display (list 'a 'b 'c 'd)) (newline)
(disp:display-all-with-tag "INFO"
                           "1st message"
                           "2nd message"
                           "3rd message")

(newline)

; varargs work for lambdas too
(display
  ((lambda (x y . z)
     (+ (* x y)
        (sum z)) ) 2 3 4 5 6) ) (newline)

; Note that because lambdas have no names, the syntax to specify no positional
; arguments is a little different: you have to specify the entire argument list
; as a single variable, not a list.
(display ((lambda ls (sum ls)) 1 2 3)) (newline)

(newline)

;; Let bindings

(define let-shadowed 4)

(display
  (let ((let-shadowed 1)
        (b 2))
    (+ let-shadowed b))) (newline)

(display let-shadowed) (newline)

; Again, we hoist definitions inside the let body, as well as multiple
; statements.

(let ((hello "hello"))
      (display hello " " world)
      (newline)

      (define world "world"))

(newline)

(define let*-shadowed-1 4)
(define let*-shadowed-2 5)

(display
  (let* ((let*-shadowed-1 1)
         (b 2)
         (let*-shadowed-2 (+ let*-shadowed-1 b)))
    let*-shadowed-2)) (newline)

(display let*-shadowed-1) (newline)
(display let*-shadowed-2) (newline)

; Again, we hoist definitions inside the let* body, as well as multiple
; statements.

(let* ((hello "hello"))
       (display hello " " world)
       (newline)

       (define world "world"))
