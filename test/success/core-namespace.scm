; "core" is imported implicitly. It is imported with the "import alL" syntax,
; so it's not necessary to prefix functions exported by "core" (such as
; "display" or "core") with the module name
(display (+ 1 2)) (newline)

; But, it's legal to do so.
(core:display (core:+ 1 2)) (newline)
