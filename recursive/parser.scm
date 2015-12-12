(require "tokens" => tok)
(require "ast")

;; PARSE LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse input)
  (if (null? input)
    '()
    (parse-expressions input)))

(define (partition pred input)
  (if (or (null? input)
          (pred (car input)))
    (cons '() input)
    (let ((first (car input))
          (partitioned-tail (partition pred (cdr input))))
      (cons (cons first (car partitioned-tail)) (cdr partitioned-tail))) ))

(define (parse-expressions input)
  ; Guaranteed to have at least one element in the input
  (define token (car input))

  (define (parse-fn-call-and-rest)
    (define parsed-prefix (parse-fn-call (cdr input)))
    (define first-parsed-expression (car parsed-prefix))
    (define parsed-prefix-rest (cdr parsed-prefix))
    (define parsed-suffix (parse parsed-prefix-rest))
    (cons
      (cons first-parsed-expression (car parsed-suffix))
      (cdr parsed-suffix)) )

  (cond ((tok:open-paren? token) (parse-fn-call-and-rest))
        (else (cons (parse-atom token) (cdr input))) ))

(define (parse-single-expression input)
  ; same as parse-expressions, but guaranteed that there is only one expression
  ; in the input
  (car (parse-expressions (list input))))

(define (parse-atom token)
  (cond ((tok:id? token) (ast:var (tok:id-get-name token)))
        ((tok:int? token) token)
        (else 'invalid)))

(define (parse-fn-call input)
  ;; open parenthesis has already been consumed
  (define partitioned (partition tok:close-paren? input))
  (define fn-call-tokens (car partitioned))
  (define rest (cdr partitioned))

  (define fn-call-function (car fn-call-tokens))
  (define fn-call-args (cdr fn-call-tokens))

  ; TODO: handle invalid input
  (cons
    (ast:function-call
      (parse-single-expression fn-call-function)
      (map parse-single-expression fn-call-args))
    ; Also consume the close parentheses
    (cdr rest)) )

(module-export
  parse)
