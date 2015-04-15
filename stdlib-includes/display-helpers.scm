(display "loading display-helpers...")
(newline)

(define (display-with-tag tag message)
  (display "[")
  (display tag)
  (display "] ")
  (display message)
  (newline))

(define (display-all-with-tag tag . messages)
  (foreach (lambda (msg) (display-with-tag tag msg)) messages) )

(define (display-non-null messages)
  (foreach (lambda (msg) (display msg) (newline))
           (filter (compose not null?) messages)) )

(module-export
  display-with-tag
  display-all-with-tag
  display-non-null)
