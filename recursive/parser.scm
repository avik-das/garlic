(require "tokens" => tok)
(require "ast")

;; PARSE LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given a list of tokens, construct an abstract syntax tree.
;;
;; @param tokens - a flat list of tokens, as produced by the lexer
;; @return the abstract syntax tree, with the nesting suggested by the tokens
(define (parse tokens)
  (let* (((tree rst) (list-to-tree tokens))
         (parsed (tree-to-ast tree)))
    (if (not (null? rst))
      (begin
        (display "[WARN] remaining unparsed tokens: ")
        (display tokens)
        (newline)
        parsed)
      parsed)))

(define (expression-to-tree tokens)
  (let (((fst . rst) tokens))
    (if (tok:open-paren? fst)
      (list-to-tree rst)
      (list fst rst)) ))

(define (list-to-tree tokens)
  (cond ((null? tokens) (list '() '()))
        ((tok:close-paren? (car tokens)) (list '() (cdr tokens)))
        (else
          (let* (((fst rst) (expression-to-tree tokens))
                 ((ls tail) (list-to-tree rst)))
            (list (cons fst ls) tail)) )))

(define (tree-to-ast tree)
  (ast:module (map subtree-to-ast tree)))

(define (subtree-to-ast tree)
  (cond ((tok:id? tree) (ast:var (tok:id-get-name tree)))
        ((tok:int? tree) (ast:int (tok:int-get-value tree)))
        ((list? tree) (subtree-to-function-call tree)) ))

(define (subtree-to-function-call tree)
  (let (((fn . args) (map subtree-to-ast tree)))
    (ast:function-call fn args)) )

(module-export
  parse)
