;; A location of a single character within a source file. Used to track what
;; part of the source is being processed currently, primarily useful for
;; reporting errors.

;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Private. Always start with the "start" constructor, then use the
;; transformation functions to create new locations relative the starting
;; location.
(define (new filename line column)
  (list filename line column))

(define (start filename)
  (new filename 1 1))

;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-filename car)
(define get-line (compose car cdr))
(define get-column (compose car cdr cdr))

;; TRANSFORMATIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (next-column loc)
  (new
    (get-filename loc)
    (get-line loc)
    (+ (get-column loc) 1)) )

(define (next-line loc)
  (new
    (get-filename loc)
    (+ (get-line loc) 1)
    1) )

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  ; Constructors
  start

  ; Getters
  get-filename
  get-line
  get-column

  ; Transformations
  next-column
  next-line)
