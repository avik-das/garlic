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

; Given a "result" object, pass it through the given list of transformations,
; short-circuiting whenever one of the transformations fails (this includes if
; the original "result" is an error).
(define (pipeline-successes result . transformers)
  (define (non-variadic result transformers)
    ; This pipeline is a bit hacky if you think about it in terms of types.
    ; Each step of the pipeline returns a potentially different type of
    ; "result" object, which can be thought of as types like Result<T0>,
    ; Result<T1>, etc. Thus, the end result should always be Result<TN>.
    ;
    ; However, as soon as one of the transformation fails, that error result is
    ; returned immediately. So possibly, this pipeline returns Result<TM>,
    ; where M < N. However, in a dynamically typed language, because Result<TM>
    ; contains an error, it is indistinguishable from Result<TN>. Just
    ; something to be aware of.
    (if (null? transformers)
        result
        (transform-success (continuation transformers) result) ))

  (define (continuation transformers)
    (let (((fn . rest-fns) transformers))
      (lambda (value) (non-variadic (fn value) rest-fns)) ))

  (non-variadic result transformers))

(define (add-error err result)
  (if (is-success? result)
      (new-with-single-error err)
      (new-error (cons err (get-errors result))) ))

(define (combine-results rs)
  (define (combine r1 rest)
    (let ((combined-rest (combine-results rest)))
      (if (is-success? combined-rest)
          (if (is-success? r1)
              (new-success (cons (get-value r1) (get-value combined-rest)))
              r1)
          (if (is-success? r1)
              combined-rest
              (new-error
                (append (get-errors r1) (get-errors combined-rest))) )) ))

  (if (null? rs)
      (new-success '())
      (combine (car rs) (cdr rs))) )

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
  pipeline-successes
  add-error
  combine-results)
