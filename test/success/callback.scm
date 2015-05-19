(require "../aux/libc_module" => c)

(define (print-args x y . rest)
  (display x " " y " " rest)
  (newline)

  (length rest))

(display (c:callme5 print-args)) (newline)
(display (c:callme6 print-args)) (newline)
