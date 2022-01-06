(require string => str)

(require "ast")
(require "compiler-error" => err)
(require "result")
(require "tokens" => tok)

;; PARSE LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Given a list of tokens, construct an abstract syntax tree.
;;
;; @param tokens - a flat list of tokens, as produced by the lexer
;; @return the abstract syntax tree, with the nesting suggested by the tokens
(define (parse tokens)
  (result:transform-success
    ; TODO - when tree-to-ast is refactored to return a "result", there will be
    ;   no need to wrap the return value in a new "result"
    (lambda (tree) (result:new-success (tree-to-ast tree)))
    (toplevel-list-to-tree tokens)))

;; TREE STRUCTURE GENERATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This phase of the parsing simply arranges the tokens into a tree structure,
;; without actually interpreting any of the tokens (other than the open and
;; close parentheses).

;; The only exception is the single quote token, which is transformed into a
;; "quote" special form. This is because the single quote is like a reader
;; macro that can affect multiple tokens after it (in the case the quote is
;; followed by a list) and therefore affects the tree structure.

(define (expression-to-tree tokens)
  (let (((fst . rest) tokens))
    (cond
      ((tok:open-paren? fst) (list-to-tree (tok:get-location fst) rest))

      ((tok:single-quote? fst)
       (result:transform-success
         (lambda (quoted-and-unquoted)
           (let (((quoted unquoted) quoted-and-unquoted))
             (result:new-success
               (list
                 (cons (tok:id (tok:get-location fst) "quote") quoted)
                 unquoted)) ))
         (expression-to-tree rest)))

      (else (result:new-success (list fst rest))) )))

(define (toplevel-list-to-tree tokens)
  (cond
    ((null? tokens) (result:new-success '()))

    ((tok:close-paren? (car tokens))
     (result:add-error
       (err:new
         (tok:get-location (car tokens))
         "unexpected closing parenthesis")
       ; Make sure to continue parsing, ignoring the extra close parenthesis.
       ; That way, if there are multiple places with too many closing
       ; parentheses, they can all be reported in one shot.
       (toplevel-list-to-tree (cdr tokens))))

    (else
      ; The only reason 'expression-to-tree' will fail is if we run out of
      ; tokens, in which case there won't be any additional parsing to be done
      ; in case of a failure.
      (result:transform-success
        (lambda (fst-and-rest)
          (let (((fst rest) fst-and-rest))
            (result:transform-success
              (lambda (remainder-list)
                (result:new-success (cons fst remainder-list)))
              (toplevel-list-to-tree rest)) ))
          (expression-to-tree tokens)) ) ))

(define (list-to-tree open-paren-loc tokens)
  (cond
    ((null? tokens)
     (result:new-with-single-error
       (err:new
         open-paren-loc
         "unterminated list (start position shown)")))

    ((tok:close-paren? (car tokens))
     (result:new-success (list '() (cdr tokens))))

    (else
      ; The only reason 'expression-to-tree' will fail is if we run out of
      ; tokens, in which case there won't be any additional parsing to be done
      ; in case of a failure.
      (result:transform-success
        (lambda (fst-and-rest)
          (let (((fst rest) fst-and-rest))
            (result:transform-success
              (lambda (list-tail-and-remaining-tokens)
                (result:new-success
                  (list
                    (cons fst (car list-tail-and-remaining-tokens))
                    (car (cdr list-tail-and-remaining-tokens)))) )
              (list-to-tree open-paren-loc rest)) ))
          (expression-to-tree tokens)) ) ))

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
               (cons
                 (tok:id (tok:get-location (car name)) "lambda")
                 (cons (cdr name) body)) )) )

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
