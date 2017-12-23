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

(define (eval-definitions definitions frame)
  (if (null? definitions)
    frame
    (let* (((first . remaining) definitions)
           (symbol (ast:definition-get-name first))
           (value (recursive-eval (ast:definition-get-body first) frame))
           (new-frame (add-to-frame frame symbol value)))
      (eval-definitions remaining new-frame)) ))

(define (eval-statement-list-without-defines statements frame)
  (if (null? statements)
    '()
    (let* (((first . remaining) statements)
           (first-result (recursive-eval first frame)))
      (if (null? remaining)
        first-result
        (eval-statement-list-without-defines remaining frame)) ) ))

(define (eval-statement-list statements frame)
  (let* ((definitions (filter ast:definition? statements))
         (non-definitions (reject ast:definition? statements))
         (new-frame (eval-definitions definitions frame)))
    (eval-statement-list-without-defines non-definitions new-frame) ))

(define (eval-module module)
  (eval-statement-list
    (ast:module-get-statements module)
    root-frame))

(module-export
  eval-module)
