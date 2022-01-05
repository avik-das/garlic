;; Defines a "result" type that holds one of two values:
;;
;; - A successful value;
;; - Or a list of errors.
;;
;; Along with this data structure, this module defines ways to compose and
;; manipulate results. For example, it's possible to easily transform a
;; successful value--if present--or to add to the list of errors (wiping out
;; any successful value if there were previously no errors).

;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-success value)
  (cons 'result-success value))

(define (new-error errs)
  (cons 'result-error errs))

(define (new-with-single-error err)
  (new-error (list err)))

;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; While okay to use directly, consider if it's possible to use a
;; transformation function, as defined below, to avoid explicit checks.

(define (is-success? result)
  (= (car result) 'result-success))

(define (is-error? result)
  (= (car result) 'result-error))

;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Assumes results are already validated to be of the correct type. Like with
;; the predicates, favor using transformation functions instead of explicit
;; destructuring. The exception is at the end of the processing when the goal
;; is to use the value or show the errors.

(define (get-value result)
  (if (is-success? result)
      (cdr result)
      '() ))

(define (get-errors result)
  (if (is-error? result)
      (cdr result)
      '() ))

;; TRANSFORMATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (transform-success transformer result)
  (if (is-success? result)
      (transformer (get-value result))
      result ))

(define (add-error err result)
  (if (is-success? result)
      (new-with-single-error err)
      (new-error (cons err (get-errors result))) ))

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  ; Constructors
  new-success
  new-error
  new-with-single-error

  ; Predicates
  is-success?
  is-error?
  
  ; Getters
  get-value
  get-errors

  ; Transformations
  transform-success
  add-error)
