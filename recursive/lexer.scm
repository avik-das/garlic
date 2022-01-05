(require string => str)

(require "result")
(require "tokens" => tok)

;; HIGH LEVEL LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lex input)
  (state-get-result (lex-possibly-empty input)) )

(define (lex-possibly-empty input)
  (if (str:null? input)
      (state-new-success '() input)
      (lex-non-empty (str:at input 0) input)) )

(define (lex-non-empty first-char input)
  (cond
    ; Ignore whitespace
    ((is-space? first-char) (lex-after-discard (consume-spaces input)))

    ; Comment
    ((str:string=? first-char ";") (lex-after-discard (consume-comment input)))

    ; Open parenthesis
    ((str:string=? first-char "(")
     (state-cons-success
       tok:open-paren
       (lex-possibly-empty (str-rest input))) )

    ; Close parenthesis
    ((str:string=? first-char ")")
     (state-cons-success
       tok:close-paren
       (lex-possibly-empty (str-rest input))) )

    ; Single quote
    ((str:string=? first-char "'")
     (state-cons-success
       tok:single-quote
       (lex-possibly-empty (str-rest input))) )

    ; Bare integer
    ((is-integer? first-char)
     (lex-after-consumption
       (consume-integer input)
       (lambda (int) (tok:int int))))

    ; Negative integer
    ((and
       (str:string=? first-char "-")
       (is-integer? (str:at input 1)))
     (lex-after-consumption
       (consume-integer (str-rest input))
       (lambda (int) (tok:int (* -1 int)))))

    ; Positive integer with explicit "+" sign
    ((and
       (str:string=? first-char "+")
       (is-integer? (str:at input 1)))
     (lex-after-consumption
       (consume-integer (str-rest input))
       (lambda (int) (tok:int int))))

    ; Identifier
    ((is-identifier-character-first? first-char)
     (lex-after-consumption
       (consume-identifier input)
       (lambda (id) (tok:id id))))

    ; Boolean
    ((str:string=? first-char "#")
     (lex-after-consumption
       (consume-boolean (str-rest input))
       (lambda (bool) (tok:bool bool))))

    ; String
    ((str:string=? first-char "\"")
     (lex-after-consumption
       (consume-string input)
       (lambda (str) (tok:str str))))

    (else
      (state-new-with-single-error
        (str:concat "Unrecognized character '" first-char "'")
        input) )))

(define (lex-after-discard state-after-discard)
  (lex-possibly-empty (state-get-rest state-after-discard)) )

(define (lex-after-consumption consumption-state token-on-success)
  (if (state-is-success? consumption-state)
      (let* ((consumption-result (state-get-result consumption-state))
             (consumption-value (result:get-value consumption-result))
             (rest (state-get-rest consumption-state)))
        (state-transform-success
          (lambda (tokens) (cons (token-on-success consumption-value) tokens))
          (lex-possibly-empty rest)) )

      ; This branch is actually a bit tricky. The consumption state technically
      ; holds a different type of value (a single token) than the lexer state
      ; (a list of tokens) in the success case. But because the consumption
      ; state is an error--and therefore has no value--it looks exactly the
      ; same as a lexer state.
      consumption-state))

;; LEXER STATE DATA STRUCTURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Both the top-level lexing (converting the string input into a list of
;; tokens) and the individual consumption functions (e.g. consuming a string to
;; form a single token) will take in and return similar data. This data is
;; represented using a "state" consisting of:
;;
;; 1. The "result" of the lexing or consumption function. The exact shape of
;;    this data will differ between the lexing (list of tokens) and consumption
;;    (single token) portions. However, any of these functions can fail, and
;;    therefore the value is wrapped in a "result" type.
;;
;; 2. The remaining string that is unconsumed or is yet to be lexed.
;;
;; A very important caveat is the state used by the lexing and consumption
;; functions are *not* the same due to the shape of the success values. Think
;; of the former as State<List<Token>> and the latter as State<Token> in a
;; strongly-typed language. However, the same data structure can be used for
;; both, it's just important to make sure to convert between the two usages of
;; that data structure as necessary.

(define (state-new result rest) (cons result rest))
(define (state-new-success value rest)
  (state-new (result:new-success value) rest))
(define (state-new-with-single-error err rest)
  (state-new (result:new-with-single-error err) rest))

(define state-get-result car)
(define state-get-rest cdr)

(define (state-is-success? state)
  (result:is-success? (state-get-result state)))

(define (state-transform-success transformer state)
  (state-new
    (result:transform-success
      (lambda (value) (result:new-success (transformer value)))
      (state-get-result state))
    (state-get-rest state)))

(define (state-cons-success head state)
  (state-transform-success
    (lambda (value) (cons head value))
    state))

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
    integer-characters
    '("?")))

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
        (else (error-and-exit "Cannot convert to integer: " chr)) ))

(define (is-string-escapable-character? chr)
  (is-char-any-of? chr '("\"" "\\" "n" "t")))

(define (char-to-escaped-character chr)
  (cond ((str:string=? chr "\"") "\"")
        ((str:string=? chr "\\") "\\")
        ((str:string=? chr  "n") "\n")
        ((str:string=? chr  "t") "\t")
        (else (error-and-exit "Cannot convert to escaped character: " chr)) ))

;; CONSUMPTION LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (str-rest str)
  (str:string-tail str 1))

(define (consume-spaces input)
  (cond ((str:null? input) (state-new-success '() input))
        ((is-space? (str:at input 0)) (consume-spaces (str-rest input)))
        (else (state-new-success '() input)) ))

(define (consume-comment input)
  (cond ((str:null? input) (state-new-success '() input))
        ((str:string=? (str:at input 0) "\n")
         (state-new-success '() (str-rest input)))
        (else (consume-comment (str-rest input))) ))

(define (consume-integer input)
  ;; Returns a list of the following:
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
    ; TODO - support returning errors
    (state-new-success int rest)) )

(define (consume-boolean input)
  (if (str:null? input)
      (state-new-with-single-error "# appears at end of file" input)

      (let ((chr (str:at input 0)))
        (cond ((str:string=? chr "t")
               (state-new-success #t (str-rest input)))

              ((str:string=? chr "f")
               (state-new-success #f (str-rest input)))

              (else
                (state-new-with-single-error
                  (str:concat "# followed by unrecognized character '" chr "'")
                  input)) )) ))

(define (consume-string input)
  (if (or (str:null? input)
          (not (str:string=? (str:at input 0) "\"")))
      (state-new-with-single-error "string does not start with \"" input)
      (helper-no-escape (str-rest input)) )

  (define (helper-no-escape input)
    (if (str:null? input)
        (state-new-with-single-error "unterminated string" input)
        (let ((chr (str:at input 0)))
          (cond
            ((str:string=? chr "\\") (helper-escaped (str-rest input)))
            ((str:string=? chr "\"")
             (state-new-success "" (str-rest input)))
            (else
              (state-transform-success
                (lambda (str) (str:concat chr str))
                (helper-no-escape (str-rest input))) )) )))

  (define (helper-escaped input)
    (if (str:null? input)
        (state-new-with-single-error "unterminated string" input)
        (let ((chr (str:at input 0)))
          (if (is-string-escapable-character? chr)
              (state-transform-success
                (lambda (str) (str:concat (char-to-escaped-character chr) str))
                (helper-no-escape (str-rest input)))

              (state-new-with-single-error
                (str:concat "invalid character after \\ in string: ")
                input)) ))) )

(define (consume-identifier input)
  (define (consume input is-acceptable-character?)
    (if (str:null? input)
        (state-new-success "" input)
        (consume-non-empty input is-acceptable-character?)))

  (define (consume-non-empty input is-acceptable-character?)
    (let ((chr (str:at input 0)))
      ; TODO - distinguish between whitespace/delimiters (e.g. parens) and
      ;   other non-identifier characters. The former should end the
      ;   consumption with a success, and the latter should result in an error.
      (if (is-acceptable-character? chr)
          (state-transform-success
            (lambda (id) (str:concat chr id))
            (consume-remaining (str-rest input)))
          (state-new-success "" input))))

  (define (consume-remaining input)
    (consume input is-identifier-character-remaining?))

  (consume input is-identifier-character-first?))

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  lex)
