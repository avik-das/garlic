(require string => str)
(require file)

(require "lexer")
(require "parser")
(require "ast")

;; TEST EVALUATOR (TEMPORARY) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Just to make sure the lexer and parser work, this extremely simple evaluator
;; "runs" the program represented by the AST produced by the parser.

(define (eval-lookup-var name)
  (cond ((str:string=? name "+") +)
        ((str:string=? name "-") -)
        ((str:string=? name "*") *)
        ((str:string=? name "display") display)
        ((str:string=? name "newline") newline) ))

(define (eval-call-function call)
  (let ((fn (ast:function-call-get-function call))
        (args (ast:function-call-get-args call)))
    (apply
      (recursive-eval fn)
      (map recursive-eval args)) ))

(define (recursive-eval tree)
  (cond ((ast:module? tree)
         (foreach recursive-eval (ast:module-get-statements tree)))
        ((ast:var? tree) (eval-lookup-var (ast:var-get-name tree)))
        ((ast:int? tree) (ast:int-get-value tree))
        ((ast:function-call? tree) (eval-call-function tree)) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (= (length *argv*) 2))
  (begin
    (display "USAGE: " (car *argv*) " <filename>")
    (newline))
  '())

(define input (file:read-text (car (cdr *argv*))) )
(define lexed (lexer:lex input))
(define parsed (parser:parse lexed))
(recursive-eval parsed)
