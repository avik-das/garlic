(require "lexer")
(require "parser")

; TODO: Start with only test input. Assume it has been lexed.
(define input "TODO")
(define lexed (lexer:lex input))

(define parsed (parser:parse lexed))
(foreach
  (lambda (expr) (display "> " expr) (newline))
  parsed) (newline)
