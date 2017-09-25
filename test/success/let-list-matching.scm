; A non-destructuring let binding. Nothing special about this one.
(let ((ls '(1 2 3)))
  (display ls) (newline))
(newline)

; A series of singly-nested destructuring let bindings.

(let (((first) '(1)))
  (display first) (newline))
(newline)

(let (((first . rest) '(1 2 3)))
  (display first) (newline)
  (display rest) (newline))
(newline)

(let (((first second . rest) '(1 2 3)))
  (display first) (newline)
  (display second) (newline)
  (display rest) (newline))
(newline)

(let (((first second third . rest) '(1 2 3)))
  (display first) (newline)
  (display second) (newline)
  (display third) (newline)
  (display rest) (newline))
(newline)

; Nested destructuring, both at the second and third level
(let (((a (b c . d) (e (f)) g) '(1 (2 3 . 4) (5 (6)) (7 . 8))))
  (display a) (newline)
  (display b) (newline)
  (display c) (newline)
  (display d) (newline)
  (display e) (newline)
  (display f) (newline)
  (display g) (newline))
(newline)

; The l-value here could be expressed as simply as (a b c), but the form used
; here exercises l-values expressed with dotted lists
(let (((a . (b . c)) '(1 2 . 3)))
  (display a) (newline)
  (display b) (newline)
  (display c) (newline))
(newline)

(let ((pair '(1 . 2)))
  (display pair) (newline))
(newline)

(let (((first . second) '(1 . 2)))
  (display first) (newline)
  (display second) (newline))
