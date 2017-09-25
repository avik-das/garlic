(define (return-list) '(1 2 3))
(define (return-cons) '(1 . 2))

(let ((ls (return-list)))
  (display ls) (newline))
(newline)

(let (((first . rest) (return-list)))
  (display first) (newline)
  (display rest) (newline))
(newline)

(let (((first second . rest) (return-list)))
  (display first) (newline)
  (display second) (newline)
  (display rest) (newline))
(newline)

(let (((first second third . rest) (return-list)))
  (display first) (newline)
  (display second) (newline)
  (display third) (newline)
  (display rest) (newline))
(newline)

(let (((first second third) (return-list)))
  (display first) (newline)
  (display second) (newline)
  (display third) (newline))
(newline)

(let ((pair (return-cons)))
  (display pair) (newline))
(newline)

(let (((first . second) (return-cons)))
  (display first) (newline)
  (display second) (newline))
