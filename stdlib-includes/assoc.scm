;; A library for working with "association lists", a common data structure in
;; Scheme. An association list is a list of pairs, where each pair has a key and
;; a value. The following operations are supported:
;;
;; - Constructing a new association list: simply create a list of pairs.
;;
;; - Retrieve an entry from an existing association list by key. Given a key,
;;   the first pair with a matching key is returned. Note that lookup takes
;;   linear time in the number of entries in the list.
;;
;; - Adding a new entry to an existing association list: simply `cons` a new
;;   pair to the front of the list. Note that if the new entry has the same key
;;   as an existing entry, the newer entry will "shadow" the later one. This
;;   operation is constant time.
;;
;; - Removing all entries with a given key from an association list. This
;;   operation takes linear time in the number of entries in the list.
;;
;; This module is loosely based on the association lists in standard Scheme
;; distributions, such as MIT Scheme [1]. The main differences are:
;;
;; - Provide constructors for association lists. The reason these are not
;;   usually provided is because association lists are simple lists of pairs,
;;   which Scheme is good at representing. However, I want to expose a more
;;   generic interface that can one day be used to power some other
;;   representation of a key->value mapping, such as a hash map.
;;
;; - Support only symbol keys. This is so we can use `=` for key comparisons. To
;;   avoid confusion with standard distributions, I've made sure not use names
;;   for the lookups that are typically associated with specific key comparison
;;   strategies (for example, `assoc` uses `equal?` for its comparisons).
;;
;; - Don't support in-place mutations.
;;
;; Any operation not implemented has not had a practical use yet.
;;
;; [1] https://www.gnu.org/software/mit-scheme/documentation/stable/mit-scheme-ref/Association-Lists.html

;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (empty) '())
(define (singleton key value) (add (empty) key value))

;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Find the first pair with the given key (first element in the pair). Returns
;; #f if no such pair is found.
(define (get-pair alist key)
  (find
    (lambda (pair) (= (car pair) key))
    alist))

;; Find the value (second element in the pair) with the given key (first element
;; in the pair). Returns #f if no such pair is found.
(define (get alist key)
  (let ((pair (get-pair alist key)))
    (if pair
      (cdr pair)
      pair) ))

;; Return a list of all the pairs in the given association list, without
;; duplicate keys. The order of the returned pairs is undefined, but the order
;; doesn't matter because there are no duplicate keys.
;;
;; The returned list is itself an association list.
(define (pairs alist)
  (define (add-if-absent pairs-so-far pair-to-add)
    (let* ((keys-so-far (map car pairs-so-far))
           (key-to-add (car pair-to-add))
           (key-is-present (any? (lambda (k) (= k key-to-add)) keys-so-far)))
    (if key-is-present
      pairs-so-far
      (cons pair-to-add pairs-so-far)) ))

  (reduce add-if-absent '() alist))

;; PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (has-key? alist key)
  (any?
    (lambda (pair) (= (car pair) key))
    alist))

;; OPERATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add alist key value)
  (cons (cons key value) alist) )

;; Given two association lists, merge the two into a single association list.
;; If the two input lists have any overlapping keys, the value in the second
;; list will take precedence over the value in the first.
(define (merge alist1 alist2)
  ; Make sure the second list comes earlier, in order for its entries to shadow
  ; the entries in the first list
  (append alist2 alist1))

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  ; Constructors
  empty
  singleton

  ; Getters
  get
  pairs

  ; Predicates
  has-key?

  ; Operations
  add
  merge)
