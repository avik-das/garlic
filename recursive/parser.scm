(require "tokens" => tok)
(require "ast")

;; PARSE LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given a list of tokens, construct an abstract syntax tree.
;;
;; @param tokens - a flat list of tokens, as produced by the lexer
;; @return the abstract syntax tree, with the nesting suggested by the tokens
(define (parse tokens)
  ; As a first step, we only construct the nesting suggested by the tokens
  ; TODO: actually assign semantics to the resulting tree
  (let (((parsed rst) (parse-list tokens)))
    (if (not (null? rst))
      (begin
        (display "[WARN] remaining unparsed tokens: ")
        (display tokens)
        (newline)
        parsed)
      parsed)))

(define (parse-expression tokens)
  (let (((fst . rst) tokens))
    (if (tok:open-paren? fst)
      (parse-list rst)
      (list fst rst)) ))

(define (parse-list tokens)
  (cond ((null? tokens) (list '() '()))
        ((tok:close-paren? (car tokens)) (list '() (cdr tokens)))
        (else
          (let* (((fst rst) (parse-expression tokens))
                 ((ls tail) (parse-list rst)))
            (list (cons fst ls) tail)) )))

(module-export
  parse)
