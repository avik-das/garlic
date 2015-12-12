(require "lexer")
(require "parser")

; TODO: Start with only test input. Assume it has been lexed.

(define input "TODO")
(define lexed (lexer:lex input))

; TODO: get back an AST
(define parsed (parser:parse lexed))
(foreach
  (lambda (expr) (display "> " expr) (newline))
  (car parsed)) (newline)
(display "rest: " (cdr parsed)) (newline)
