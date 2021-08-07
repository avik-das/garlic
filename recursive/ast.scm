;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (module statements)
  (cons 'garlic-module statements))

(define (var name)
  (cons 'var name))

(define (int val)
  (cons 'int val))

(define (atom val)
  (cons 'atom val))

(define (str val)
  (cons 'str val))

(define (quoted-list ls)
  (cons 'list ls))

(define (definition name body)
  (list 'definition name body))

; Represents a lambda
(define (function args body)
  (list 'function args body))

(define (function-call fn args)
  (list 'function-call fn args))

;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (starts-with-symbol? ast symbol)
  (and (list? ast)
       (symbol? (car ast))
       (= (car ast) symbol)))

(define (type-checker type)
  (lambda (ast) (starts-with-symbol? ast type)) )

(define module? (type-checker 'garlic-module))
(define var? (type-checker 'var))
(define int? (type-checker 'int))
(define atom? (type-checker 'atom))
(define str? (type-checker 'str))
(define quoted-list? (type-checker 'list))
(define definition? (type-checker 'definition))
(define function? (type-checker 'function))
(define function-call? (type-checker 'function-call))

;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assumes AST nodes are already validated to be of the correct type

(define module-get-statements cdr)
(define var-get-name cdr)
(define int-get-value cdr)
(define atom-get-name cdr)
(define str-get-value cdr)
(define quoted-list-get-list cdr)

(define definition-get-name (compose car cdr))
(define definition-get-body (compose car cdr cdr))

(define function-get-args (compose car cdr))
(define function-get-body (compose car cdr cdr))

(define function-call-get-function (compose car cdr))
(define function-call-get-args (compose car cdr cdr))

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  ; Constructors
  module
  var
  int
  atom
  str
  quoted-list
  definition
  function
  function-call

  ; Predicates
  module?
  var?
  int?
  atom?
  str?
  quoted-list?
  definition?
  function?
  function-call?

  ; Getters
  module-get-statements
  var-get-name
  int-get-value
  atom-get-name
  str-get-value
  quoted-list-get-list
  definition-get-name
  definition-get-body
  function-get-args
  function-get-body
  function-call-get-function
  function-call-get-args)
