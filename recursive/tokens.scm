;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (id loc name)
  (list 'id loc name))

(define (int loc value)
  (list 'int loc value))

(define (bool loc value)
  (list 'bool loc value))

(define (str loc value)
  (list 'str loc value))

(define (open-paren loc) (list 'open-paren loc))
(define (close-paren loc) (list 'close-paren loc))
(define (single-quote loc) (list 'single-quote loc))

;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-token-of-compound-type? token type)
  (and (list? token)
       (symbol? (car token))
       (= (car token) type)) )

(define (id? token)
  (is-token-of-compound-type? token 'id) )

(define (int? token)
  (is-token-of-compound-type? token 'int) )

(define (bool? token)
  (is-token-of-compound-type? token 'bool) )

(define (str? token)
  (is-token-of-compound-type? token 'str) )

(define (open-paren? token)
  (is-token-of-compound-type? token 'open-paren))

(define (close-paren? token)
  (is-token-of-compound-type? token 'close-paren))

(define (single-quote? token)
  (is-token-of-compound-type? token 'single-quote))

;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assumes tokens are already validated to be of the correct type

(define get-location (compose car cdr))

(define id-get-name (compose car cdr cdr))
(define int-get-value (compose car cdr cdr))
(define bool-get-value (compose car cdr cdr))
(define str-get-value (compose car cdr cdr))

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  ; Constructors
  id
  int
  bool
  str
  open-paren
  close-paren
  single-quote
  
  ; Predicates
  id?
  int?
  bool?
  str?
  open-paren?
  close-paren?
  single-quote?
  
  ; Getters
  get-location
  id-get-name
  int-get-value
  bool-get-value
  str-get-value)
