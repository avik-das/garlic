(require "tokens" => tok)

(define (lex input)
  ; TODO
  (list
    tok:open-paren
    (tok:id "+")
    (tok:int 1)
    (tok:int 2)
    tok:close-paren
    tok:open-paren
    (tok:id "+")
    (tok:int 3)
    (tok:int 4)
    tok:close-paren))

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  lex)
