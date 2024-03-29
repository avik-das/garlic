(require string => str)
(require file)

(require "compiler-error" => err)
(require "location" => loc)
(require "result")

(require "lexer")
(require "parser")
(require "static-analyzer")
(require "evaluator")

(require "compiler_utils" => compiler-utils)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (= (length *argv*) 2))
  (begin
    (display "USAGE: " (car *argv*) " <filename>")
    (newline))
  '())

(define (show-errors input errs)
  (define (show-repeated-chars n chr)
    (if (= n 0)
        '()
        (begin
          (display chr)
          (show-repeated-chars (- n 1) chr)) ))

  (define (num-digits-for-positive-int int)
    ; Put a limit of 99,999 for the number. This is to avoid needing to
    ; implement division and rounding, or other methods of determining the
    ; number of digits in a number.
    (cond ((> int 10000) 5)
          ((> int  1000) 4)
          ((> int   100) 3)
          ((> int    10) 2)
          (else 1) ))

  (define (show-single-error err)
    (let* ((loc (err:get-location err))
           (line (loc:get-line loc))
           (column (loc:get-column loc))

           (msg (err:get-message err))

           (num-chars-in-lineno (num-digits-for-positive-int line)))
      (display
        "  "
        "ERROR: "
        msg
        " ("
        (compiler-utils:filename-from-path (loc:get-filename loc))
        ":"
        line
        ":"
        column
        ")")

      (newline)
      (newline)

      (display
        "    "
        line
        "| "
        (compiler-utils:line-from-file-contents input line))

      (newline)

      (display "    ")
      (show-repeated-chars num-chars-in-lineno " ")
      (display "  ")
      (show-repeated-chars (- column 1) "-")
      (display "^")

      (newline)))

  (define (show-remaining-errors errs)
    (if (null? errs)
        '()
        (begin
          (newline)
          (show-single-error (car errs))
          (show-remaining-errors (cdr errs))) ))

  (let ((num-errors (length errs)))
    (display
      "Compilation failed ("
      num-errors
      " "
      (if (> num-errors 1) "errors" "error")
      ")")
    (newline))

  (show-remaining-errors errs))

(define filename (car (cdr *argv*)))
(define input (file:read-text filename) )

(define final-result
  (result:pipeline-successes
    (result:new-success input)
    (lambda (input) (lexer:lex filename input))
    (lambda (lexed) (parser:parse lexed))
    (lambda (module) (static-analyzer:analyze-module module))
    (lambda (module)
      (evaluator:eval-module module)
      (result:new-success '())) ))

(if (result:is-error? final-result)
    (begin
      (show-errors input (result:get-errors final-result))
      (newline)
      (error-and-exit "COMPILATION FAILED"))
    '() ) ; Otherwise: continue
