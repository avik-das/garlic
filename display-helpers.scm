(display "loading display-helpers...")
(newline)

(define (display-with-tag tag message)
        (display "[")
        (display tag)
        (display "] ")
        (display message)
        (newline))

(module-export
  display-with-tag)
