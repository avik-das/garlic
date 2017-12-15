(require string => str)
(require file)

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

(define input (file:read-text (car (cdr *argv*))) )
(define lexed (lexer:lex input))
(define parsed (parser:parse lexed))
(evaluator:eval-module parsed)
