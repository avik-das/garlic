;; An implementation of an interpreter for an AST with the interfaces provided
;; by the "ast" module. This AST is assumed to have been produced from some
;; input source code, but the details of that are irrelevant. The evaluator
;; simply executes the code represented by the AST.

(require string => str)

(require "ast")

;; FRAME ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-frame parent symbols)
  (cons parent symbols))

(define frame-parent car)
(define frame-symbols cdr)

(define root-frame
  (make-frame '()
              (list (cons "+" +)
                    (cons "-" -)
                    (cons "*" *)
                    (cons "display" display)
                    (cons "newline" newline))) )

(define (find-in-frame frame symbol)
  (define (find-in-list ls)
    (find
      (lambda (entry) (str:string=? (car entry) symbol))
      ls))

  (if (null? frame)
    '()
    (let* ((parent (frame-parent frame))
           (symbols (frame-symbols frame))
           (entry (find-in-list symbols)))
      (if entry
        (cdr entry)
        (find-in-frame parent symbol)) )) )

(define (add-to-frame frame symbol value)
  (let* ((parent (frame-parent frame))
         (symbols (frame-symbols frame))
         (entry (cons symbol value))
         (new-symbols (cons entry symbols)))
    (make-frame parent new-symbols) ))

;; EVALUATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval-call-function call frame)
  (let ((fn (ast:function-call-get-function call))
        (args (ast:function-call-get-args call)))
    ; In the future, instead of calling `apply`, a new frame will be
    ; constructed, and the body of the function will be evaluated with respect
    ; to that frame.
    (apply
      (recursive-eval fn frame)
      (map (recursive-eval-with-frame frame) args)) ))

(define (recursive-eval tree frame)
  (cond ((ast:var? tree) (find-in-frame frame (ast:var-get-name tree)))
        ((ast:int? tree) (ast:int-get-value tree))
        ((ast:function-call? tree) (eval-call-function tree frame)) ))

(define (recursive-eval-with-frame frame)
  (lambda (tree)
    (recursive-eval tree frame)) )

(define (eval-module module)
  (foreach
    (recursive-eval-with-frame root-frame)
    (ast:module-get-statements module) ))

(module-export
  eval-module)
