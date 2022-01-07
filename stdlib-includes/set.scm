;; A "set" data structure where every element appears exactly once. The
;; equality of elements in the set are determined by a user-supplied equality
;; function, and convenience constructors are provided when the equality
;; function is simply "=".
;;
;; Note that, in order to achieve simplicity of implementation, this data
;; structure implements inefficient operations. For example, containment checks
;; are O(N), meaning the "add-all" operator is O(N*M). Do not depend on this
;; data structure for large data sets.

;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new-with-equality fn= items) (cons fn= items))
(define (new items) (new-with-equality = items))

(define (empty-with-equality fn=) (new-with-equality fn= '()))
(define empty (empty-with-equality =))

;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-fn= car)
(define get-items cdr)

;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (contains? set item)
  (let ((fn= (get-fn= set))
        (items (get-items set)))
    (any? (lambda (x) (fn= x item)) items) ))

;; OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Internal - create a new set with all the same properties as the original set
; (namely the same equality function), but with the given items.
(define (update-items set new-items)
  (new-with-equality
    (get-fn= set)
    new-items) )

(define (add-all set items-to-add)
  (let ((items-not-in-set (reject (lambda (x) (contains? set x)) items-to-add))
        (set-items (get-items set)))
    (update-items set (append set-items items-not-in-set)) ))

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  ; Constructors
  empty
  empty-with-equality
  new
  new-with-equality

  ; Predicates
  contains?

  ; Operations
  add-all)
