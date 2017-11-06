;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (module statements)
  (cons 'garlic-module statements))

(define (var name)
  (cons 'var name))

(define (int val)
  (cons 'int val))

(define (function-call fn args)
  (list 'function-call fn args))

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  ; Constructors
  module
  var
  int
  function-call)
