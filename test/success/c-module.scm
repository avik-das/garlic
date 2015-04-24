; libctest is a C module, not a Garlic one.
(require "../aux/libc_module")

; We can call arbitrary methods inside the C module, passing it parameters and
; getting back values that can be composed with other, Garlic functions.
(display (libc_module:add 1 2)) (newline)
(display (+ (libc_module:add 3 4) 5)) (newline)

; This tests that parameters that spill out of the registers are passed
; properly on the stack.
(display (libc_module:lastarg12 1 2 3 4 5 6 7 8 9 10 11 12)) (newline)

; The arguments can be arbitrary expressions.
(display (libc_module:add
                ((lambda (x) (+ x 1)) 2)
                (libc_module:lastarg12 1 2 3 4 5 6 7 8 9 10 11 24)) )
(newline)

; All the extra arguments are ignored.
(display (libc_module:add 1 2 3 4 5 6 7 8 9 10 11 12)) (newline)

(newline)

; Comprehensive tests based on number of arguments (from none to 6, the max
; number of arguments passed in registers, as well as more than 6, which causes
; spilling onto the stack).
(display (libc_module:arg0)) (newline)
(display (libc_module:arg1 1)) (newline)
(display (libc_module:arg2 1 2)) (newline)
(display (libc_module:arg3 1 2 3)) (newline)
(display (libc_module:arg4 1 2 3 4)) (newline)
(display (libc_module:arg5 1 2 3 4 5)) (newline)
(display (libc_module:arg6 1 2 3 4 5 6)) (newline)
(display (libc_module:arg8 1 2 3 4 5 6 7 8)) (newline)
