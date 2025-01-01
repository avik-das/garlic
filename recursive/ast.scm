;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (module statements)
  (cons 'garlic-module statements))

(define (var loc name)
  (list 'var loc name))

(define (int loc val)
  (list 'int loc val))

(define (atom loc val)
  (list 'atom loc val))

(define (bool loc val)
  (list 'bool loc val))

(define (str loc val)
  (list 'str loc val))

(define (quoted-list loc ls)
  (list 'list loc ls))

(define (definition loc name body)
  (list 'definition loc name body))

(define (conditional-clause loc condition body-statements)
  (list 'cond-clause loc condition body-statements))

(define (conditional-else loc body-statements)
  (list 'cond-else loc body-statements))

(define (conditional loc clauses)
  (list 'conditional loc clauses))

; Represents a lambda
(define (function loc args body)
  (list 'function loc args body))

(define (function-call loc fn args)
  (list 'function-call loc fn args))

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
(define bool? (type-checker 'bool))
(define str? (type-checker 'str))
(define quoted-list? (type-checker 'list))
(define definition? (type-checker 'definition))
(define conditional? (type-checker 'conditional))
(define conditional-clause? (type-checker 'cond-clause))
(define conditional-else? (type-checker 'cond-else))
(define function? (type-checker 'function))
(define function-call? (type-checker 'function-call))

;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Assumes AST nodes are already validated to be of the correct type

;; All AST nodes will have the location in a standardized place within its
;; representation, so the location can be retrieved without knowing the type of
;; AST node.
(define get-location (compose car cdr))

(define module-get-statements cdr)
(define var-get-name (compose car cdr cdr))
(define int-get-value (compose car cdr cdr))
(define atom-get-name (compose car cdr cdr))
(define bool-get-value (compose car cdr cdr))
(define str-get-value (compose car cdr cdr))
(define quoted-list-get-list (compose car cdr cdr))

(define definition-get-name (compose car cdr cdr))
(define definition-get-body (compose car cdr cdr cdr))

(define conditional-get-clauses (compose car cdr cdr))
(define conditional-clause-get-condition (compose car cdr cdr))
(define conditional-clause-get-body-statements (compose car cdr cdr cdr))
(define conditional-else-get-body-statements (compose car cdr cdr))

(define function-get-args (compose car cdr cdr))
(define function-get-body (compose car cdr cdr cdr))

(define function-call-get-function (compose car cdr cdr))
(define function-call-get-args (compose car cdr cdr cdr))

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  ; Constructors
  module
  var
  int
  atom
  bool
  str
  quoted-list
  definition
  conditional-clause
  conditional-else
  conditional
  function
  function-call

  ; Predicates
  module?
  var?
  int?
  atom?
  bool?
  str?
  quoted-list?
  definition?
  conditional-clause?
  conditional-else?
  conditional?
  function?
  function-call?

  ; Getters
  get-location

  module-get-statements
  var-get-name
  int-get-value
  atom-get-name
  bool-get-value
  str-get-value
  quoted-list-get-list
  definition-get-name
  definition-get-body
  conditional-get-clauses
  conditional-clause-get-condition
  conditional-clause-get-body-statements
  conditional-else-get-body-statements
  function-get-args
  function-get-body
  function-call-get-function
  function-call-get-args)
