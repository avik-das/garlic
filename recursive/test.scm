;; Adds the three numberical inputs.
(define (addthreenums a b c)
    ; Using the function definition syntax here
    (+ a (+ b c)) )

;; Adds the four numberical inputs. This demonstrates nested lambdas, and the
;; fact that variable shadowing works. The four variables are bound in all
;; manner of ways inside the inner lambdas, but there's no conflict because
;; each lambda introduces its own scope.
(define addfournums
  (lambda (a b c d)
    (+ ((lambda (d c) (+ d c)) a b)
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

;; Defined variables can have all sorts of names
(define ALL-CAPS 111) (display ALL-CAPS) (newline)
(define with-numbers-123 112) (display with-numbers-123) (newline)
(define *with-stars* 113) (display *with-stars*) (newline)

(newline)

(display (addthreenums c b a)) (newline)
(display (+ a 9)) (newline) ; Prove that "a" is not blown away by calling
                            ; addthreenums

(display (addfournums 1 2 3 4)) (newline)
(display ((closure 1) 2) ) (newline)

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
(display
  (*  ; comments
    1 ; inside
    2 ; complex
    3 ; expressions
    4 ; !
    )) (newline)

(display 0x1) (newline)
(display 0x01) (newline)
(display 0xff) (newline)
(display 0x100) (newline)

(newline)

(display 0xF) (newline)
(display 0x1A) (newline)
(display 0xDEADBEEF) (newline)

(newline)

(display (+ 0xff 1)) (newline)
(display (- 0xff 0xf)) (newline)

(newline)

(define (display-with-newline msg)
  (display msg)
  (newline))

(display-with-newline 123)
(display-with-newline 456)

;; Quoted values

(newline)

(display 'a) (newline)
(display '123) (newline)
(display '#t) (newline)
(display '#f) (newline)
(display '(1 2 3)) (newline)
(display '(1 (2 3) 4)) (newline)
(display '(this is a list)) (newline)

;; Nil

(newline)

(display '()) (newline)
(display (display 'a)) (newline)

;; Booleans and conditionals.

(newline)

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

; Cond expressions

(newline)

(define (check-1-2-3 n)
  (cond ((= n 1) 'one)
        ((= n 2) 'two)
        ((= n 3) 'three)
        (else 'none-of-the-above)) )

(display (check-1-2-3 1)) (newline)
(display (check-1-2-3 2)) (newline)
(display (check-1-2-3 3)) (newline)
(display (check-1-2-3 4)) (newline)

;; Strings

(newline)

(display "a string") (newline)
(display "some numbers and symbols: 1 2 3 # @ <- cool!") (newline)
(display "a string with a newline afterwards\n")
(display "a string with\n  a newline in the middle") (newline)
(display "another string with
  a newline in the middle") (newline)
(display "\ta string with an embedded tab") (newline)
(display "a string with \"escaped\" quotes") (newline)
(display "no need to 'escape' single quotes") (newline)
(display "") (newline)
(display "^ empty string above") (newline)
