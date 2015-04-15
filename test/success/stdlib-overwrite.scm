; "filter" is provided by stdlib, and therefore it's imported into the current
; scope automatically. However, we can overwrite it if desired.
(define (filter f ls)
  '())

(define not-null? (compose not null?))
(display (filter not-null? '(1 () 2 () 3))) (newline)

; However, the original stdlib function is still available using an explicit
; name space.
(display (stdlib:filter not-null? '(1 () 2 () 3))) (newline)

; Furthermore, inside of stdlib, "filter" continues to refer to the original
; stdlib function. For example, "reject" is provided by stdlib and it depends
; on "filter". Despite not using an explitic namespace, stdlib will continue to
; use the original function.
(display (reject null? '(1 () 2 () 3))) (newline)
