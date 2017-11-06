(require "tokens" => tok)

(define (lex input)
  ; TODO
  (list
    ; (display (+ (- 1 2) (* 3 (+ 4 5)) 6))
    tok:open-paren
    (tok:id "display")
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
    (tok:id "+")
    (tok:int 4)
    (tok:int 5)
    tok:close-paren
    tok:close-paren
    (tok:int 6)
    tok:close-paren
    tok:close-paren

    ; (newline)
    tok:open-paren
    (tok:id "newline")
    tok:close-paren

    ; (display (+ 7 8))
    tok:open-paren
    (tok:id "display")
    tok:open-paren
    (tok:id "+")
    (tok:int 7)
    (tok:int 8)
    tok:close-paren
    tok:close-paren

    ; (newline)
    tok:open-paren
    (tok:id "newline")
    tok:close-paren

    ; (display 9)
    tok:open-paren
    (tok:id "display")
    (tok:int 9)
    tok:close-paren

    ; (newline)
    tok:open-paren
    (tok:id "newline")
    tok:close-paren))

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  lex)
