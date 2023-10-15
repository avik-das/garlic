; Allow top-level comments
;; Starting with two semicolons
 ; Starting after a space
    ; Starting after many spaces
;     With lots of spaces

(display "line comment") ; Allow inline commentS
(newline)                ; At end of line
                         ; Followed by top-level comment

( ; Allow comment inside function call
 ; On its own line
 display ; After the function
  ; On its own line again
  "comment inside function call" ; After an argument
  ; Breaking up the end parentheses
  ) (newline)

(let ; Comment
  ( ; Lots
   ( ; of
    a ; comments
    1 ; all
   ) ; over
  ) ; the
  ; place
  (display a) ; all
  (newline) ; ignored
  ; properly
)

; Repeat for let* and letrec

(let* ; Comment
  ( ; Lots
   ( ; of
    a ; comments
    2 ; all
   ) ; over
  ) ; the
  ; place
  (display a) ; all
  (newline) ; ignored
  ; properly
)

(letrec ; Comment
  ( ; Lots
   ( ; of
    a ; comments
    3 ; all
   ) ; over
  ) ; the
  ; place
  (display a) ; all
  (newline) ; ignored
  ; properly
)

( ; At the beginning
 if ; and
 #t ; all
 (display 11) ; over
 (display 12) ; the
 ; place
)
(newline)

(display
  ( ; Inside a cond
    ; Lots
    cond ; and
    ; lots
    ( ; and
     ; lots
     ( ; and
      ; lots
      = ; and
      ; lots
      1 ; and
      ; lots
      2 ; and
      ; lots
     ) ; and
     ; lots
     13 ; and
     ; lots
    ) ; and
    ; lots
    ( ; and
     ; lots
     ( ; and
      ; lots
      = ; and
      ; lots
      1 ; and
      ; lots
      1 ; and
      ; lots
     ) ; and
     ; lots
     14 ; and
     ; lots
    ) ; and
    ; lots
    ( ; and
     ; lots
     else ; and
     ; lots
     15 ; and
     ; lots
    ) ; and
    ; of
  ) ; comments!
)
(newline)

(;Comment
 ; TODO: comments are not currently allowed without a leading space inside of
 ;   lists :(
 display
 "allow comments without space separator"
);Comment
(newline)

(display
  '(;Comment
   21 ; Comment
   ;Comment
   .
   22
  )
);Comment
(newline)
