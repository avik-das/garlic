(display "first list is okay")

(display
  "second list is not okay"
  '(1 2 3)
  '(4 5 6) ; <- no closing paren for 'display'
(newline)
