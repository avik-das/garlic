; This file contains a series of top-level statements that are supported by the
; codegen. Each statement is codegened in such a way as to leave the result in
; %rax. Whatever is the last such value in %rax is the returned status code of
; the entire execution.
;
; Thus, only the last top-level statement in this file has any observable
; behavior. You can simply return comment out any lines after the one you want
; to observe.

; Strings are an exception. Unlike numerical results, strings are immediately
; printed out to stdout, and the result is a constant `0`. Thus, putting a
; string at the end of the file will cause the executable to return `0` as the
; status code.
"abcd\n"
"Hello, world!\n"

205
#t  ; represented as 2
#f  ; represented as 4
'#t ; quoted booleans evaluate to their corresponding booleans
'#f
'() ; represented as 0

(if #t 1 2)
(cond
  (#t 10)
  (#f 20)
  (else 30))
