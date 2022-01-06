;; An error that can be produced by any part of the compilation process. Most
;; notably, the error contains:
;;
;; - A user-visible message.
;; - The location of the error.
;;
;; Thus, no matter how source locations are passed around, however errors are
;; reported, there is one clear way to actually show the error to the user.

(require string => str)

;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (new loc . msg-parts)
  (cons loc (apply str:concat msg-parts)) )

;; GETTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-location car)
(define get-message cdr)

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  ; Constructors
  new

  ; Getters
  get-location
  get-message)
