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

(display (list? '(1))) (newline)
(display (list? '(1 2))) (newline)
(display (list? (list 1 2))) (newline)
(display (list? '())) (newline)
(display (list? 'symbol)) (newline)
(display (list? 1)) (newline)

(newline)

(display (number? 1)) (newline)
(display (number? -100)) (newline)
(display (number? 0xFF)) (newline)
(display (number? 1.2)) (newline)
(display (number? 'symbol)) (newline)
(display (number? '())) (newline)
(display (number? '(1 2))) (newline)
