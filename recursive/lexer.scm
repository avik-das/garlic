(require string => str)

(require "compiler-error" => err)
(require "location" => loc)
(require "result")
(require "tokens" => tok)

;; HIGH LEVEL LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (lex filename input)
  (state-get-result (lex-possibly-empty (loc:start filename) input)) )

(define (lex-possibly-empty loc input)
  (if (str:null? input)
      (state-new-success '() loc input)
      (lex-non-empty (str:at input 0) loc input)) )

(define (lex-non-empty first-char loc input)
  (cond
    ; Ignore whitespace
    ((is-space? first-char)
     (lex-after-discard (consume-spaces loc input)))

    ; Comment
    ((str:string=? first-char ";")
     (lex-after-discard (consume-comment loc input)))

    ; Open parenthesis
    ((str:string=? first-char "(")
     (state-cons-success
       (tok:open-paren loc)
       (lex-possibly-empty (loc:next-column loc) (str-rest input))) )

    ; Close parenthesis
    ((str:string=? first-char ")")
     (state-cons-success
       (tok:close-paren loc)
       (lex-possibly-empty (loc:next-column loc) (str-rest input))) )

    ; Single quote
    ((str:string=? first-char "'")
     (state-cons-success
       (tok:single-quote loc)
       (lex-possibly-empty (loc:next-column loc) (str-rest input))) )

    ; Bare hexadecimal integer
    ((and
       (str:string=? first-char "0")
       (or
         (str:string=? (str:at input 1) "x")
         (str:string=? (str:at input 1) "X")))
     (lex-after-consumption
       (consume-hex-integer
         (loc:next-column (loc:next-column loc))
         (str-rest (str-rest input)))
       (lambda (int) (tok:int loc int))))

    ; Bare integer
    ((is-integer? first-char)
     (lex-after-consumption
       (consume-integer loc input)
       (lambda (int) (tok:int loc int))))

    ; Negative integer
    ((and
       (str:string=? first-char "-")
       (is-integer? (str:at input 1)))
     (lex-after-consumption
       (consume-integer (loc:next-column loc) (str-rest input))
       (lambda (int) (tok:int loc (* -1 int)))))

    ; Positive integer with explicit "+" sign
    ((and
       (str:string=? first-char "+")
       (is-integer? (str:at input 1)))
     (lex-after-consumption
       (consume-integer (loc:next-column loc) (str-rest input))
       (lambda (int) (tok:int loc int))))

    ; Identifier
    ((is-identifier-character-first? first-char)
     (lex-after-consumption
       (consume-identifier loc input)
       (lambda (id) (tok:id loc id))))

    ; Boolean
    ((str:string=? first-char "#")
     (lex-after-consumption
       (consume-boolean (loc:next-column loc) (str-rest input))
       (lambda (bool) (tok:bool loc bool))))

    ; String
    ((str:string=? first-char "\"")
     (lex-after-consumption
       (consume-string loc input)
       (lambda (str) (tok:str loc str))))

    (else
      (state-new-with-single-error
        (err:new loc "Unrecognized character '" first-char "'")
        loc
        input) )))

(define (lex-after-discard state-after-discard)
  (lex-possibly-empty
    (state-get-location state-after-discard)
    (state-get-rest state-after-discard)) )

(define (lex-after-consumption consumption-state token-on-success)
  (if (state-is-success? consumption-state)
      (let* ((consumption-result (state-get-result consumption-state))
             (consumption-value (result:get-value consumption-result))
             (loc (state-get-location consumption-state))
             (rest (state-get-rest consumption-state)))
        (state-transform-success
          (lambda (tokens) (cons (token-on-success consumption-value) tokens))
          (lex-possibly-empty loc rest)) )

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

(define (state-new result loc rest) (list result loc rest))
(define (state-new-success value loc rest)
  (state-new (result:new-success value) loc rest))
(define (state-new-with-single-error err loc rest)
  (state-new (result:new-with-single-error err) loc rest))

(define state-get-result car)
(define state-get-location (compose car cdr))
(define state-get-rest (compose car cdr cdr))

(define (state-is-success? state)
  (result:is-success? (state-get-result state)))

(define (state-transform-success transformer state)
  (state-new
    (result:transform-success
      (lambda (value) (result:new-success (transformer value)))
      (state-get-result state))
    (state-get-location state)
    (state-get-rest state)))

(define (state-cons-success head state)
  (state-transform-success
    (lambda (value) (cons head value))
    state))

;; CHARACTER MATCHING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-char-any-of? chr ls)
  (any? (lambda (test) (str:string=? chr test)) ls))

(define space-characters
  '(" " "\t" "\n" "\r"))

(define (is-space? chr)
  (is-char-any-of? chr space-characters))

(define delimeter-characters
  (append
    space-characters
    '("(" ")" ";")))

(define (is-delimeter? chr)
  (is-char-any-of? chr delimeter-characters))

(define integer-characters
  '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))

(define hex-integer-characters
  '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
    "a" "b" "c" "d" "e" "f"
    "A" "B" "C" "D" "E" "F"))

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

(define (is-hex-integer? chr)
  (is-char-any-of? chr hex-integer-characters))

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

(define (hex-char-to-int chr)
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
        ((or (str:string=? chr "a") (str:string=? chr "A")) 10)
        ((or (str:string=? chr "b") (str:string=? chr "B")) 11)
        ((or (str:string=? chr "c") (str:string=? chr "C")) 12)
        ((or (str:string=? chr "d") (str:string=? chr "D")) 13)
        ((or (str:string=? chr "e") (str:string=? chr "E")) 14)
        ((or (str:string=? chr "f") (str:string=? chr "F")) 15)
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

(define (consume-spaces loc input)
  (cond
    ((str:null? input) (state-new-success '() loc input))

    ; TODO - handle "\r\n"
    ((str:string=? (str:at input 0) "\n")
     (consume-spaces (loc:next-line loc) (str-rest input)))

    ((is-space? (str:at input 0))
     (consume-spaces (loc:next-column loc) (str-rest input)))

    (else (state-new-success '() loc input)) ))

(define (consume-comment loc input)
  (cond
    ((str:null? input) (state-new-success '() loc input))

    ((str:string=? (str:at input 0) "\n")
     (state-new-success '() (loc:next-line loc) (str-rest input)))

    (else (consume-comment (loc:next-column loc) (str-rest input))) ))

(define (consume-integer loc input)
  ;; Returns a list of the following:
  ;;
  ;;    1. The parsed integer consisting of:
  ;;
  ;;       a. The preceding integer portion (multiplied by the appropriate
  ;;          power due to it being at the left of the remainder).
  ;;
  ;;       b. The remainder of the integer at the beginning of the input.
  ;;
  ;;    2. The new source location at the start of any unparsed remaining part
  ;;       of the string.
  ;;
  ;;    3. Any unparsed remaining part of the string.
  ;;
  ;;    4. The power of ten representing the order of magnitude of the parsed
  ;;       remainder (i.e. disregarding the preceding part). For example, if
  ;;       the remainder contains one, two or three digits, then the power is
  ;;       "1", "10", or "100" respectively.
  (define (helper loc input preceding)
    (if (str:null? input)
        (list preceding loc "" 1)
        (let ((chr (str:at input 0)))
          (if (is-integer? chr)
            (let* ((next-loc (loc:next-column loc))
                   ((int rest-loc rest power)
                    (helper next-loc (str-rest input) (char-to-int chr)))
                   (new-power (* power 10)))
              (list (+ (* preceding new-power) int) rest-loc rest new-power))
            (list preceding loc input 1))) ))

  (let (((int new-loc rest _) (helper loc input 0)))
    (if (or (str:null? rest)
            (is-delimeter? (str:at rest 0)))
        (state-new-success int new-loc rest)

        (state-new-with-single-error
          (err:new
            new-loc
            "unexpected character when parsing integer: '" (str:at rest 0) "'")
          loc
          input)) ))

;; See documentation for consume-integer. This function works almos the same
;; way, but parses hexadecimal digits instead of decimal digits.
(define (consume-hex-integer loc input)
  (define (helper loc input preceding)
    (if (str:null? input)
        (list preceding loc "" 1)
        (let ((chr (str:at input 0)))
          (if (is-hex-integer? chr)
            (let* ((next-loc (loc:next-column loc))
                   ((int rest-loc rest power)
                    (helper next-loc (str-rest input) (hex-char-to-int chr)))
                   (new-power (* power 16)))
              (list (+ (* preceding new-power) int) rest-loc rest new-power))
            (list preceding loc input 1))) ))

  (let (((int new-loc rest _) (helper loc input 0)))
    (if (or (str:null? rest)
            (is-delimeter? (str:at rest 0)))
        (state-new-success int new-loc rest)

        (state-new-with-single-error
          (err:new
            new-loc
            "unexpected character when parsing hexadecimal integer: '"
            (str:at rest 0)
            "'")
          loc
          input)) ))

(define (consume-boolean loc input)
  (if (str:null? input)
      (state-new-with-single-error
        (err:new loc "# appears at end of file")
        loc
        input)

      (let ((chr (str:at input 0))
            (next-loc (loc:next-column loc)))
        (cond ((str:string=? chr "t")
               (state-new-success #t next-loc (str-rest input)))

              ((str:string=? chr "f")
               (state-new-success #f next-loc (str-rest input)))

              (else
                (state-new-with-single-error
                  (err:new
                    loc
                    "# followed by unrecognized character '" chr "'")
                  loc
                  input)) )) ))

(define (consume-string start-loc input)
  (if (or (str:null? input)
          (not (str:string=? (str:at input 0) "\"")))
      (state-new-with-single-error
        (err:new start-loc "string does not start with \"")
        start-loc
        input)
      (helper-no-escape (loc:next-column start-loc) (str-rest input)) )

  (define (helper-no-escape loc input)
    (if (str:null? input)

        (state-new-with-single-error
          (err:new
            start-loc
            "unterminated string (start position shown)")
          loc
          input)

        (let ((chr (str:at input 0)))
          (cond
            ((str:string=? chr "\\")
             (helper-escaped (loc:next-column loc) (str-rest input)))

            ((str:string=? chr "\"")
             (state-new-success "" (loc:next-column loc) (str-rest input)))

            ; TODO - handle "\r\n"
            ((str:string=? chr "\n")
              (state-transform-success
                (lambda (str) (str:concat chr str))
                (helper-no-escape (loc:next-line loc) (str-rest input))) )

            (else
              (state-transform-success
                (lambda (str) (str:concat chr str))
                (helper-no-escape
                  (loc:next-column loc)
                  (str-rest input))) )) )))

  (define (helper-escaped loc input)
    (if (str:null? input)

        (state-new-with-single-error
          (err:new
            start-loc
            "unterminated string (start position shown)")
          loc
          input)

        (let ((chr (str:at input 0)))
          (if (is-string-escapable-character? chr)

              (state-transform-success
                (lambda (str) (str:concat (char-to-escaped-character chr) str))
                (helper-no-escape (loc:next-column loc) (str-rest input)))

              (state-new-with-single-error
                (err:new loc "invalid character after \\ in string: ")
                loc
                input)) ))) )

(define (consume-identifier loc input)
  (define (consume loc input is-acceptable-character?)
    (if (str:null? input)
        (state-new-success "" loc input)
        (consume-non-empty loc input is-acceptable-character?)))

  (define (consume-non-empty loc input is-acceptable-character?)
    (let ((chr (str:at input 0)))
      ; TODO - distinguish between whitespace/delimiters (e.g. parens) and
      ;   other non-identifier characters. The former should end the
      ;   consumption with a success, and the latter should result in an error.
      (if (is-acceptable-character? chr)
          (state-transform-success
            (lambda (id) (str:concat chr id))
            (consume-remaining (loc:next-column loc) (str-rest input)))
          (state-new-success "" loc input))))

  (define (consume-remaining loc input)
    (consume loc input is-identifier-character-remaining?))

  (consume loc input is-identifier-character-first?))

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  lex)
