;; +
(display (+)) (newline) ; returns a fixnum by default
(display (+ 1)) (newline) ; no conversion
(display (+ 1.0)) (newline) ; no conversion
(display (+ 1 2)) (newline) ; no conversion
(display (+ 1.0 2)) (newline) ; convert to float
(display (+ 1 2.0)) (newline) ; convert to float
(display (+ 1.0 2 3.0 4 5.0)) (newline) ; convert to float

(newline)

;; *
(display (*)) (newline) ; returns a fixnum by default
(display (* 1)) (newline) ; no conversion
(display (* 1.0)) (newline) ; no conversion
(display (* 1 2)) (newline) ; no conversion
(display (* 1.0 2)) (newline) ; convert to float
(display (* 1 2.0)) (newline) ; convert to float
(display (* 1.0 2 3.0 4 5.0)) (newline) ; convert to float

(newline)

;; -
(display (- 1)) (newline) ; no conversion
(display (- 1.0)) (newline) ; no conversion
(display (- 1 2)) (newline) ; no conversion
(display (- 1.0 2)) (newline) ; convert to float
(display (- 1 2.0)) (newline) ; convert to float
(display (- 1.0 2 3.0 4 5.0)) (newline) ; convert to float

(newline)

;; nested and combinations
(display (+ (* 1 2) 3)) (newline) ; no conversion
(display (+ (* 1.0 2) 3)) (newline) ; convert to float
(display (+ (* 1 2) 3.0)) (newline) ; convert to float
