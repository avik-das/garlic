(display '()) (newline)
(display '(a)) (newline)
(display '(a b)) (newline)
(display '(a b c)) (newline)
(display '(a b c d)) (newline)

(newline)

(display (list)) (newline)
(display (list 'a)) (newline)
(display (list 'a 'b)) (newline)
(display (list 'a 'b 'c)) (newline)
(display (list 'a 'b 'c 'd)) (newline)

(newline)

(display '(())) (newline)
(display '(a ())) (newline)
(display '(a () b)) (newline)
(display '(a (b) c)) (newline)
(display '(a (b c) ((d)))) (newline)
(display '((a b) (c d) (e))) (newline)

(newline)

(display (list (list))) (newline)
(display (list 'a (list))) (newline)
(display (list 'a (list) 'b)) (newline)
(display (list 'a (list 'b) 'c)) (newline)
(display (list 'a (list 'b 'c) (list (list 'd)))) (newline)
(display (list (list 'a 'b) (list 'c 'd) (list 'e))) (newline)

(newline)

(display (list '(1 2 3) '(a b c))) (newline)
