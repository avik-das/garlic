(define (print-and-true)
  (display "returning true... ")
  #t)

(define (print-and-false)
  (display "returning false... ")
  #f)

;; AND ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; booleans
(display (and #t)) (newline)
(display (and #t #t)) (newline)
(display (and #t #f)) (newline)
(display (and #t #t #t)) (newline)
(display (and #t #f #t)) (newline)

(newline)

; truthy vs. falsey values
(display (and '() #t)) (newline)
(display (and #f #t)) (newline)
(display (and '(1) #t)) (newline)
(display (and 1 #t)) (newline)

(newline)

; short circuiting
(display (and (print-and-true) (print-and-false))) (newline)
(display (and (print-and-false) (print-and-true))) (newline)

(newline)
(newline)

;; AND ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; booleans
(display (or #t)) (newline)
(display (or #t #t)) (newline)
(display (or #t #f)) (newline)
(display (or #f #f)) (newline)
(display (or #t #t #t)) (newline)
(display (or #f #f #f)) (newline)

(newline)

; truthy vs. falsey values
(display (or '() #f)) (newline)
(display (or #f #f)) (newline)
(display (or '(1) #f)) (newline)
(display (or 1 #f)) (newline)

(newline)

; short circuiting
(display (or (print-and-true) (print-and-false))) (newline)
(display (or (print-and-false) (print-and-true))) (newline)
