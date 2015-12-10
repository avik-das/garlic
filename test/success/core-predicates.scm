(display (null? '())) (newline)
(display (null? (list))) (newline)
(display (null? '(1))) (newline)
(display (null? '(1 2))) (newline)
(display (null? 'symbol)) (newline)
(display (null? 1)) (newline)

(newline)

(display (symbol? 'symbol)) (newline)
(display (symbol? '())) (newline)
(display (symbol? 1)) (newline)

(newline)

(display (cons? '(1))) (newline)
(display (cons? '(1 2))) (newline)
(display (cons? (list 1 2))) (newline)
(display (cons? '())) (newline)
(display (cons? 'symbol)) (newline)
(display (cons? 1)) (newline)
