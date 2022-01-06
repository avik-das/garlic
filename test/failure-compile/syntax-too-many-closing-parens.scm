(display "first list is okay")

(display
  "second list is not okay"
  '(1 2 3)
  '(4 5 6))) ; <- extra closing paren for 'display'
(newline)

(display
  "third list is also not okay"
  '(7 8 9))))) ; <- extra closing parens for 'display'
(newline)

(display "fourth list is okay")
