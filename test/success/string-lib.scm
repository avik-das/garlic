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

(newline)

(display (str:at "abcd" 0)) (newline)
(display (str:at "abcd" 1)) (newline)
(display (str:at "abcd" 2)) (newline)
(display (str:at "abcd" 3)) (newline)
(display (str:at "abcd" 4)) (newline)

(newline)

(display (str:string-tail "abcd" 0)) (newline)
(display (str:string-tail "abcd" 1)) (newline)
(display (str:string-tail "abcd" 2)) (newline)
(display (str:string-tail "abcd" 3)) (newline)
(display (str:string-tail "abcd" 4)) (newline)

(newline)

(display (str:null? "not-empty")) (newline)
(display (str:null? "")) (newline)

(newline)

(display (str:length "")) (newline)
(display (str:length "a")) (newline)
(display (str:length "ab")) (newline)
(display (str:length "abc")) (newline)
(display (str:length "abcd")) (newline)

(newline)

(display (str:to-bytes "abcd")) (newline)
(display (str:to-bytes "1234\t\nabcd\\\"")) (newline)
