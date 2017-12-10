(require string => str)
(require "tokens" => tok)

;; HIGH LEVEL LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lex input)
  (if (str:null? input)
    '()
    (lex-non-empty (str:at input 0) input) ))

(define (lex-non-empty first-char input)
  (cond
    ((is-space? first-char) (lex (consume-spaces input)))

    ((str:string=? first-char "(")
     (cons tok:open-paren (lex (str-rest input))) )

    ((str:string=? first-char ")")
     (cons tok:close-paren (lex (str-rest input))) )

    ((is-integer? first-char)
     (let (((int . rest) (consume-integer input 0)))
       (cons (tok:int int) (lex rest))) )

    ((is-identifier-character? first-char)
     (let (((id . rest) (consume-identifier input)))
       (cons (tok:id id) (lex rest))) ) ))

;; CHARACTER MATCHING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-char-any-of? chr ls)
  (any? (lambda (test) (str:string=? chr test)) ls))

(define (is-space? chr)
  (is-char-any-of? chr '(" " "\t" "\n" "\r")))

; TODO: handle numerical characters after the first one
(define (is-identifier-character? chr)
  (is-char-any-of?
    chr
    '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
      "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
      "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
      "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
      "-" "+" "*" "/" "_" "=" "<" ">") ))

(define (is-integer? chr)
  (is-char-any-of? chr '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9")))

(define (char-to-int chr)
  (cond ((str:string=? chr "0") 0)
        ((str:string=? chr "1") 1)
        ((str:string=? chr "2") 2)
        ((str:string=? chr "3") 3)
        ((str:string=? chr "4") 4)
        ((str:string=? chr "5") 5)
        ((str:string=? chr "6") 6)
        ((str:string=? chr "7") 7)
        ((str:string=? chr "8") 8)
        ((str:string=? chr "9") 9)
        (else 0) ))

;; CONSUMPTION LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (str-rest str)
  (str:string-tail str 1))

(define (consume-spaces input)
  (cond ((str:null? input) input)
        ((is-space? (str:at input 0)) (consume-spaces (str-rest input)))
        (else input) ))

(define (consume-integer input preceding)
  (if (str:null? input)
    preceding
    (let ((chr (str:at input 0)))
      (if (is-integer? chr)
        (let (((int . rest)
               (consume-integer (str-rest input) (char-to-int chr))))
          (cons (+ (* preceding 10) int) rest))
        (cons preceding input))) ))

(define (consume-identifier input)
  (if (str:null? input)
    ""
    (let ((chr (str:at input 0)))
      (if (is-identifier-character? chr)
        (let (((id . rest) (consume-identifier (str-rest input))))
          (cons (str:concat chr id) rest))
        (cons "" input))) ))

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  lex)
