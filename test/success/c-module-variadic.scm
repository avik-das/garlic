(require "../aux/libc_variadic_module" => v)

; Non-variadic functions work whether or not they were exported with the
; "is_vararg" set.
(display (v:non_variadic_add_no_flag 1 2)) (newline)
(display (v:non_variadic_add_flag 3 4)) (newline)

(newline)

; Variadic functions work when there are extra arguments passed to them.
(display (v:variadic0_add 1 2 3 4)) (newline)
(display (v:variadic1_add 1 2 3 4)) (newline)
(display (v:variadic9_add 1 2 3 4 5 6 7 8 9 10 11)) (newline)

(newline)

; Variadic functions work when there are no extra arguments passed to them.
(display (v:variadic0_add)) (newline)
(display (v:variadic1_add 1)) (newline)
(display (v:variadic9_add 1 2 3 4 5 6 7 8 9)) (newline)
