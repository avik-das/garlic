; libctest is a C module, not a scheme one.
(require "../aux/libc_module")

; We can call arbitrary methods inside the C module, passing it parameters and
; getting back values that can be composed with other, Scheme functions.
(display (ccall libc_module:add 1 2)) (newline)
(display (+ (ccall libc_module:add 3 4) 5)) (newline)

; This tests that parameters that spill out of the registers are passed
; properly on the stack.
(display (ccall libc_module:lastarg12 1 2 3 4 5 6 7 8 9 10 11 12)) (newline)

; The arguments can be arbitrary expressions.
(display (ccall libc_module:add
                ((lambda (x) (+ x 1)) 2)
                (ccall libc_module:lastarg12 1 2 3 4 5 6 7 8 9 10 11 24)) )
(newline)

; All the extra arguments are ignored.
(display (ccall libc_module:add 1 2 3 4 5 6 7 8 9 10 11 12)) (newline)
