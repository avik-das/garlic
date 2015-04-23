(define (to-word n)
  (cond ((= n 0) 'one)
        ((= n 1) 'two)))

(display (to-word 0)) (newline)
(display (to-word 1)) (newline)
(display (to-word 2)) (newline)
