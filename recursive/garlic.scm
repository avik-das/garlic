(require string => str)
(require file)

(require "result")

(require "lexer")
(require "parser")
(require "ast")
(require "evaluator")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (= (length *argv*) 2))
  (begin
    (display "USAGE: " (car *argv*) " <filename>")
    (newline))
  '())

(define (show-errors errs)
  (define (show-remaining-errors errs)
    (if (null? errs)
        '()
        (begin
          (newline)
          (display "  " (car errs)) ; TODO: show errors with context
          (newline)
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

(define input (file:read-text (car (cdr *argv*))) )
(define lexed (lexer:lex input))

(if (result:is-error? lexed)
    (begin
      (show-errors (result:get-errors lexed))
      (newline)
      (error-and-exit "COMPILATION FAILED"))
    '() ) ; Otherwise: continue

(define parsed (parser:parse (result:get-value lexed)))
(evaluator:eval-module parsed)
