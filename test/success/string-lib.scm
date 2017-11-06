(require string => str)

(display (str:concat)) (newline)
(display (str:concat "a")) (newline)
(display (str:concat "a" "bc")) (newline)
(display (str:concat "a" "bc" "def")) (newline)
(display (str:concat "a" "" "bc" "" "def")) (newline)

(newline)

(display (str:concat-list '())) (newline)
(display (str:concat-list (list "a"))) (newline)
(display (str:concat-list (list "a" "bc"))) (newline)
(display (str:concat-list (list "a" "bc" "def"))) (newline)
(display (str:concat-list (list "a" "" "bc" "" "def"))) (newline)

(newline)

(display (str:symbol->str 'this-was-a-symbol)) (newline)

(newline)

(display (str:string=? "abc" "abc")) (newline)
(display (str:string=? "ABC" "abc")) (newline)
