;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (id name)
  (cons 'id name))

(define (int value)
  (cons 'int value))

(define (str value)
  (cons 'str value))

(define open-paren 'open-paren)
(define close-paren 'close-paren)
(define single-quote 'quote)

;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (is-token-of-compound-type? token type)
  (and (list? token)
       (symbol? (car token))
       (= (car token) type)) )

(define (is-token-of-primitive-type? token type)
  (and (symbol? token)
       (= token type)))

(define (id? token)
  (is-token-of-compound-type? token 'id) )

(define (int? token)
  (is-token-of-compound-type? token 'int) )

(define (str? token)
  (is-token-of-compound-type? token 'str) )

(define (open-paren? token)
  (is-token-of-primitive-type? token open-paren))

(define (close-paren? token)
  (is-token-of-primitive-type? token close-paren))

(define (single-quote? token)
  (is-token-of-primitive-type? token single-quote))

;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assumes tokens are already validated to be of the correct type

(define id-get-name cdr)
(define int-get-value cdr)
(define str-get-value cdr)

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  ; Constructors
  id
  int
  str
  open-paren
  close-paren
  single-quote
  
  ; Predicates
  id?
  int?
  str?
  open-paren?
  close-paren?
  single-quote?
  
  ; Getters
  id-get-name
  int-get-value
  str-get-value)
