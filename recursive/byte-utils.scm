(define (int->little-endian int pad-to-total-bytes)
  ; 0x12345678 -> 0x78 0x56 0x34 0x12
  ; 0x00005678 -> 0x78 0x56 0x00 0x00
  (define (convert-remaining-bytes reduced-int num-bytes-left)
    (if (= num-bytes-left 0)
      '()
      (cons
        (bitwise-and reduced-int 0xff)
        (convert-remaining-bytes
          (arithmetic-shift reduced-int -8)
          (- num-bytes-left 1)) ) ))

  (convert-remaining-bytes int pad-to-total-bytes))

(module-export
  int->little-endian)
