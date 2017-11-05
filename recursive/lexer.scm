(require "tokens" => tok)

(define (lex input)
  ; TODO
  (list
    ; (+ (- 1 2) (* 3 (/ 4 5)) 6)
    tok:open-paren
    (tok:id "+")
    tok:open-paren
    (tok:id "-")
    (tok:int 1)
    (tok:int 2)
    tok:close-paren
    tok:open-paren
    (tok:id "*")
    (tok:int 3)
    tok:open-paren
    (tok:id "/")
    (tok:int 4)
    (tok:int 5)
    tok:close-paren
    tok:close-paren
    (tok:int 6)
    tok:close-paren

    ; (+ 7 8)
    tok:open-paren
    (tok:id "+")
    (tok:int 7)
    (tok:int 8)
    tok:close-paren

    ; 9
    (tok:int 9)))

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  lex)
