(require string => str)

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
    (cond
      ((tok:open-paren? fst) (list-to-tree rst))

      ((tok:single-quote? fst)
       (let (((quoted unquoted) (expression-to-tree rst)))
         (list (cons (tok:id "quote") quoted) unquoted)) )

      (else (list fst rst)) )))

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
        ((tok:bool? tree) (ast:bool (tok:bool-get-value tree)))
        ((tok:str? tree) (ast:str (tok:str-get-value tree)))
        ((list? tree) (specialize-subtree tree)) ))

(define (specialize-subtree tree)
  (define (is-type? type name)
    (and (tok:id? type)
         (str:string=? (tok:id-get-name type) name)) )

  ; Assumes `tree` is a list
  (let ((type (car tree)))
    (cond ((is-type? type "define") (subtree-to-define tree))
          ((is-type? type "lambda") (subtree-to-lambda tree))
          ((is-type? type "quote") (subtree-to-quoted tree))
          ((is-type? type "if") (subtree-if-to-cond tree))
          ((is-type? type "cond") (subtree-cond-to-cond tree))
          (else (subtree-to-function-call tree)) ) ))

(define (subtree-to-define tree)
  (let (((keyword name . body) tree))
    (cond ((tok:id? name)
           ; The first case is a simple value definition:
           ;
           ;   (define name body)
           ;
           ; In this case, only one statement is supported in the "body", so
           ; the body is assumed to be a single element list.
           (ast:definition
             (tok:id-get-name name)
             (subtree-to-ast (car body))) )

          ((and
             (list? name)
             (not (null? name))
             (tok:id? (car name)))
           ; The second case is if the name is a list:
           ;
           ;   (define (function-name arg0 arg1 ...) ...)
           ;           ^---------- name -----------^
           ;
           ; This represents a function definition, and it should be
           ; transformed to a value definition in which the value is a lambda:
           ;
           ;   (define function-name (lambda (arg0 arg 1) ...))
           (ast:definition
             (tok:id-get-name (car name))
             (subtree-to-lambda
               ; Synthesize a list of tokens representing a lambda. Notice that
               ; the body, which is a list of trees, is the tail of the lambda
               ; list, as opposed to the last element.
               (cons (tok:id "lambda") (cons (cdr name) body)) )) )

          (else
            (display "\033[1;31m" name "\033[0m") (newline)
            (error-and-exit "Invalid definition name ^")) )))

(define (subtree-to-lambda tree)
  ; Does not support variadic functions yet. Thus, it is assumed the argument
  ; list of the lambda is a flat list of identifiers.
  (let (((keyword args . statements) tree))
    (ast:function
      (map tok:id-get-name args)
      (map subtree-to-ast statements)) ))

(define (subtree-to-quoted tree)
  (define (helper to-quote)
    (cond
      ((null? to-quote) (ast:quoted-list '()))

      ((tok:id? to-quote) (ast:atom (tok:id-get-name to-quote)))
      ((tok:int? to-quote) (ast:int (tok:int-get-value to-quote)))
      ((tok:bool? to-quote) (ast:bool (tok:bool-get-value to-quote)))

      ((list? to-quote)
       (ast:quoted-list (map helper to-quote)))

      (else
        (display "\033[1;31m" to-quote "\033[0m") (newline)
        (error-and-exit "ERROR - invalid quoted value ^")) ))

  (helper (cdr tree)))

(define (subtree-if-to-cond tree)
  (let (((keyword condition true-clause false-clause) tree))
    (ast:conditional
      (list
        (ast:conditional-clause
          (subtree-to-ast condition)
          (list (subtree-to-ast true-clause)))
        (ast:conditional-else (list (subtree-to-ast false-clause))))) ))

(define (subtree-cond-to-cond tree)
  (define (subtree-to-clause subtree)
    (let* (((condition . body-statements) subtree)
           (body-statements-ast (map subtree-to-ast body-statements)))
      (if (and (tok:id? condition)
               (str:string=? (tok:id-get-name condition) "else"))
          (ast:conditional-else body-statements-ast)
          (ast:conditional-clause
            (subtree-to-ast condition)
            body-statements-ast)) ))

  (ast:conditional
    (map subtree-to-clause (cdr tree))) )

(define (subtree-to-function-call tree)
  (let (((fn . args) (map subtree-to-ast tree)))
    (ast:function-call fn args)) )

(module-export
  parse)
