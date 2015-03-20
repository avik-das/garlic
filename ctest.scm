; libctest is a C module, not a scheme one
(require libctest)

(display (ccall libctest:add 1 2)) (newline)
(display (+ (ccall libctest:add 3 4) 5)) (newline)

; all the extra arguments are ignored
(display (ccall libctest:add 1 2 3 4 5 6 7 8 9 10 11 12)) (newline)
