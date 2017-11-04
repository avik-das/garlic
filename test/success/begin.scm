(begin
  (display "beginning new block:") (newline)
  (display 1) (newline)
  (display 2) (newline))

(newline)

(display (+ 1
            (begin (display "beginning new block:") (newline)
                   (display 1) (newline)
                   2))) (newline)

(newline)

(display
  (if #t
    (begin
      (display "true") (newline)
      1)
    (begin
      (display "false") (newline)
      2))) (newline)
