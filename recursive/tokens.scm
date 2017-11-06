;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (id name)
  (cons 'id name))

(define (int value)
  (cons 'int value))

(define open-paren 'open-paren)
(define close-paren 'close-paren)

;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (id? token)
  (and (list? token)
       (= (car token) 'id)) )

(define (int? token)
  (and (list? token)
       (= (car token) 'int)) )

(define (open-paren? token)
  (and (symbol? token)
       (= token 'open-paren)))

(define (close-paren? token)
  (and (symbol? token)
       (= token 'close-paren)))

;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assumes tokens are already validated to be of the correct type

(define id-get-name cdr)
(define int-get-value cdr)

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  ; Constructors
  id
  int
  open-paren
  close-paren
  
  ; Predicates
  id?
  int?
  open-paren?
  close-paren?
  
  ; Getters
  id-get-name
  int-get-value)
