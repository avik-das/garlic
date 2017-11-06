;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (module statements)
  (cons 'garlic-module statements))

(define (var name)
  (cons 'var name))

(define (int val)
  (cons 'int val))

(define (function-call fn args)
  (list 'function-call fn args))

;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (starts-with-symbol? ast symbol)
  (and (list? ast)
       (symbol? (car ast))
       (= (car ast) symbol)))

(define (module? ast)
  (starts-with-symbol? ast 'garlic-module))

(define (var? ast)
  (starts-with-symbol? ast 'var))

(define (int? ast)
  (starts-with-symbol? ast 'int))

(define (function-call? ast)
  (starts-with-symbol? ast 'function-call))

;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assumes AST nodes are already validated to be of the correct type

(define module-get-statements cdr)
(define var-get-name cdr)
(define int-get-value cdr)

(define function-call-get-function (compose car cdr))
(define function-call-get-args (compose car (compose cdr cdr)))

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  ; Constructors
  module
  var
  int
  function-call

  ; Predicates
  module?
  var?
  int?
  function-call?

  ; Getters
  module-get-statements
  var-get-name
  int-get-value
  function-call-get-function
  function-call-get-args)
