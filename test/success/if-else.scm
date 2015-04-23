(define my-not
  (lambda (val) (if val #f #t)))

(display (if #t 'correct 'wrong)) (newline)
(display (if #f 'wrong 'correct)) (newline)
(display (if '() 'correct 'wrong)) (newline)
(display (if 0 'correct 'wrong)) (newline)
(display (if 1 'correct 'wrong)) (newline)
(display (if (my-not #f) 'correct 'wrong)) (newline)
(display (if (my-not #t) 'wrong 'correct)) (newline)
(display (if (my-not 0) 'wrong 'correct)) (newline)
(display (if (my-not 1) 'wrong 'correct)) (newline)
