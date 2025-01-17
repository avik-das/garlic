(display (+)) (newline)
(display (+ 1)) (newline)
(display (+ 1 2)) (newline)
(display (+ 1 2 3)) (newline)
(display (+ 1 2 3 4)) (newline)
(display (+ 1 2 3 4 5)) (newline)

(newline)

(display (- 1)) (newline)
(display (- 2 1)) (newline)
(display (- 3 2 1)) (newline)
(display (- 4 3 2 1)) (newline)
(display (- 5 4 3 2 1)) (newline)

(newline)

(display (*)) (newline)
(display (* 1)) (newline)
(display (* 1 -2)) (newline)
(display (* 1 -2 3)) (newline)
(display (* 1 -2 3 -4)) (newline)
(display (* 1 -2 3 -4 5)) (newline)

(newline)

(display (>  1  2)) (newline)
(display (>  1 -2)) (newline)
(display (> -1  2)) (newline)
(display (> -1 -2)) (newline)
(display (<  1  2)) (newline)
(display (<  1 -2)) (newline)
(display (< -1  2)) (newline)
(display (< -1 -2)) (newline)

;; BITWISE ARITHMETIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display (bitwise-and 0x12345678 0xff      )) (newline)
(display (bitwise-and 0x12345678 0xff00    )) (newline)
(display (bitwise-and 0x12345678 0xff0000  )) (newline)
(display (bitwise-and 0x12345678 0xff000000)) (newline)

; 1111 1110 1100 1000 0000 => fec80
; 1001 1001 1001 1001 1001 => 99999
; ------------------------
; 1111 1111 1101 1001 1001 => ffd99
(display (bitwise-ior 0xfec80 0x99999)) (newline)

; NOTE: 0b1001 == 9
(display (arithmetic-shift 9  2)) (newline)
(display (arithmetic-shift 9  1)) (newline)
(display (arithmetic-shift 9  0)) (newline)
(display (arithmetic-shift 9 -1)) (newline)
(display (arithmetic-shift 9 -2)) (newline)
(display (arithmetic-shift 9 -3)) (newline)
(display (arithmetic-shift 9 -4)) (newline)
(display (arithmetic-shift 9 -5)) (newline)
