;; Like add.scm, this tests variadic functions. However, in this test, the
;; order of the parameters matters, and so, accidentally pulling out the
;; parameters in reverse order will not be acceptable.

(display (- 1)) (newline)
(display (- 1 2)) (newline)
(display (- 1 2 3)) (newline)
(display (- 1 2 3 4)) (newline)
(display (- 1 2 3 4 5)) (newline)
