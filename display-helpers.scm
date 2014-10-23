(require stdlib)

(display "loading display-helpers...")
(newline)

(define (display-with-tag tag message)
        (display "[")
        (display tag)
        (display "] ")
        (display message)
        (newline))

(define (display-non-null messages)
  (stdlib:foreach (lambda (msg) (display msg) (newline))
           (stdlib:filter (stdlib:compose stdlib:not null?) messages)) )

(module-export
  display-with-tag
  display-non-null)
