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
                    (cons "cons" cons)
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

;; LAMBDA DATA STRUCTURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-lambda parent-frame args body)
  (list 'lambda parent-frame args body))

(define (is-lambda? fn)
  (and (list? fn)
       (symbol? (car fn))
       (= (car fn) 'lambda)))

(define lambda-get-parent-frame (compose car cdr))
(define lambda-get-args (compose car cdr cdr))
(define lambda-get-body (compose car cdr cdr cdr))

;; EVALUATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eval-call-function call frame)
  (let* ((fn (recursive-eval (ast:function-call-get-function call) frame))
         (args
           (map (recursive-eval-with-frame frame)
                (ast:function-call-get-args call))) )
    (if (is-lambda? fn)
      (eval-call-lambda fn args)
      (apply fn args) )))

(define (eval-call-lambda fn args)
  (define (add-args-to-frame names args frame)
    (if (null? names)
      frame
      (add-args-to-frame
        (cdr names)
        (cdr args)
        (add-to-frame frame (car names) (car args))) ))

  (let* ((parent-frame (lambda-get-parent-frame fn))
         (empty-frame (make-frame parent-frame '()))
         (frame-with-args
           (add-args-to-frame (lambda-get-args fn) args empty-frame))
         (body (lambda-get-body fn)))
    (eval-statement-list body frame-with-args) ))

(define (recursive-eval tree frame)
  (cond ((ast:var? tree) (find-in-frame frame (ast:var-get-name tree)))
        ((ast:int? tree) (ast:int-get-value tree))
        ((ast:bool? tree) (ast:bool-get-value tree))
        ((ast:str? tree) (ast:str-get-value tree))
        ; For now, just treat an atom like a string, i.e. don't share the value
        ; across multiple usages.
        ((ast:atom? tree) (ast:atom-get-name tree))
        ((ast:quoted-list? tree)
         (map
           (lambda (subtree) (recursive-eval subtree frame))
           (ast:quoted-list-get-list tree)))
        ((ast:function? tree) (ast-function-to-lambda tree frame))
        ((ast:function-call? tree) (eval-call-function tree frame)) ))

(define (recursive-eval-with-frame frame)
  (lambda (tree)
    (recursive-eval tree frame)) )

(define (ast-function-to-lambda fn frame)
  (let ((args (ast:function-get-args fn))
        (body (ast:function-get-body fn)))
    (create-lambda frame args body) ))

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
