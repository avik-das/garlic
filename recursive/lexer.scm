(require string => str)
(require "tokens" => tok)

;; HIGH LEVEL LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lex input)
  (if (str:null? input)
    '()
    (lex-non-empty (str:at input 0) input) ))

(define (lex-non-empty first-char input)
  (cond
    ; Ignore whitespace
    ((is-space? first-char) (lex (consume-spaces input)))

    ; Comment
    ((str:string=? first-char ";") (lex (consume-comment input)))

    ; Open parenthesis
    ((str:string=? first-char "(")
     (cons tok:open-paren (lex (str-rest input))) )

    ; Close parenthesis
    ((str:string=? first-char ")")
     (cons tok:close-paren (lex (str-rest input))) )

    ; Bare integer
    ((is-integer? first-char)
     (let (((int . rest) (consume-integer input)))
       (cons (tok:int int) (lex rest))) )

    ; Negative integer
    ((and
       (str:string=? first-char "-")
       (is-integer? (str:at input 1)))
     (let (((int . rest) (consume-integer (str-rest input))))
       (cons (tok:int (* -1 int)) (lex rest))) )

    ; Positive integer with explicit "+" sign
    ((and
       (str:string=? first-char "+")
       (is-integer? (str:at input 1)))
     (let (((int . rest) (consume-integer (str-rest input))))
       (cons (tok:int int) (lex rest))) )

    ; Identifier
    ((is-identifier-character-first? first-char)
     (let (((id . rest) (consume-identifier input)))
       (cons (tok:id id) (lex rest))) )

    (else
      (error-and-exit "ERROR - could not parse:\n\n" input)) ))

;; CHARACTER MATCHING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-char-any-of? chr ls)
  (any? (lambda (test) (str:string=? chr test)) ls))

(define (is-space? chr)
  (is-char-any-of? chr '(" " "\t" "\n" "\r")))

(define integer-characters
  '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(define identifier-characters-first
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
    "-" "+" "*" "/" "_" "=" "<" ">"))

(define identifier-characters-remaining
  (append
    identifier-characters-first
    integer-characters))

(define (is-identifier-character-first? chr)
  (is-char-any-of?
    (str:downcase chr)
    identifier-characters-first))

(define (is-identifier-character-remaining? chr)
  (is-char-any-of?
    (str:downcase chr)
    identifier-characters-remaining))

(define (is-integer? chr)
  (is-char-any-of? chr integer-characters))

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

(define (consume-comment input)
  (cond ((str:null? input) input)
        ((str:string=? (str:at input 0) "\n") (str-rest input))
        (else (consume-comment (str-rest input))) ))

(define (consume-integer input)
  ;; returns a list of the following:
  ;;
  ;;    1. The parsed integer consisting of:
  ;;
  ;;       a. The preceding integer portion (multiplied by the appropriate
  ;;          power due to it being at the left of the remainder).
  ;;
  ;;       b. The remainder of the integer at the beginning of the input.
  ;;
  ;;    2. Any unparsed remaining part of the string.
  ;;
  ;;    3. The power of ten representing the order of magnitude of the parsed
  ;;       remainder (i.e. disregarding the preceding part). For example, if
  ;;       the remainder contains one, two or three digits, then the power is
  ;;       "1", "10", or "100" respectively.
  (define (helper input preceding)
    (if (str:null? input)
        (list preceding "" 1)
        (let ((chr (str:at input 0)))
          (if (is-integer? chr)
            (let* (((int rest power)
                    (helper (str-rest input) (char-to-int chr)))
                   (new-power (* power 10)))
              (list (+ (* preceding new-power) int) rest new-power))
            (list preceding input 1))) ))

  (let (((int rest _) (helper input 0)))
    (cons int rest)) )

(define (consume-identifier input)
  (define (consume input is-acceptable-character?)
    (if (str:null? input)
        (cons "" input)
        (let ((chr (str:at input 0)))
          (if (is-acceptable-character? chr)
              (let (((id . rest) (consume-remaining (str-rest input))))
                (cons (str:concat chr id) rest))
              (cons "" input))) ))

  (define (consume-remaining input)
    (consume input is-identifier-character-remaining?))

  (consume input is-identifier-character-first?))


;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  lex)
