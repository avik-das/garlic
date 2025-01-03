;; Convert a signed (negative, positive or zero) integer into its twos
;; complement unsigned counterpart, given a certain byte-length for the final
;; result. The result is guaranteed to be non-negative.
;;
;; Assumes the given integer is in the correct range for the specified byte
;; length. For example, for one byte conversions, the input integer must be in
;; the range [-128, 127]. Note that in the range [128, 255], this conversion can
;; also be thought of keeping an unsigned integer as unsigned, i.e. no
;; conversion is performed. Thus, for a specified byte range, this function can
;; handle both signed and unsigned inputs correctly.
;;
;; Examples
;;
;;   1 byte:
;;        0 =>   0 (= 0x00)
;;        1 =>   1 (= 0x01)
;;      127 => 127 (= 0x7f)
;;      255 => 255 (= 0xff)  <-- input is unsigned, kept as unsigned
;;       -1 => 255 (= 0xff)
;;     -128 => 128 (= 0x80)
;;
;;    4 bytes:
;;        0 =>          0 (= 0x00000000)
;;        1 =>          1 (= 0x00000001)
;;       -1 => 2147483648 (= 0xffffffff)
;;
;; @param sint - the signed integer to convert
;; @param num-bytes - the number of bytes in the two's complement
;;                    representation. Relevant especially for negative inputs.
;; @result a non-negative integer corresponding to the two's-complement
;;         representation of the given signed integer
(define (signed-int->twos-complement sint num-bytes)
  (define (compute-base)
    (define (helper base remaining-num-bytes)
      (if (= remaining-num-bytes 0)
        base
        (helper
          (bitwise-ior (arithmetic-shift base 8) 0xff)
          (- remaining-num-bytes 1)) ))

    (helper 0 num-bytes))

  (if (or (= sint 0)
          (> sint 0))
    ; Already positive
    sint

    ; Negative -> convert to two's complement
    (let ((base (compute-base)))
      (- base (- (- sint) 1))) ))

;; Convert the given signed integer into a list of bytes representing the
;; integer in little-endian (least-significant byte first) order. Any negative
;; integers are first converted to two's complement.
;;
;; Assumes the given integer fits into either the signed or unsigned range for
;; the specified number of bytes.
;;
;; Examples (4 bytes):
;;
;;    0x12345678 -> 0x78 0x56 0x34 0x12
;;    0x00005678 -> 0x78 0x56 0x00 0x00
;;   -0x00005678 -> 0x88 0xa9 0xff 0xff
;;
;; @param int - the signed integer to convert
;; @param pad-to-total-bytes - the total number of bytes in the output list
;; @return a list of bytes corresponding to the little-endian two's complement
;;         representation of `int`. Guaranteed to have `pad-to-total-bytes`
;;         elements.
(define (int->little-endian int pad-to-total-bytes)
  (define (convert-remaining-bytes reduced-int num-bytes-left)
    (if (= num-bytes-left 0)
      '()
      (cons
        (bitwise-and reduced-int 0xff)
        (convert-remaining-bytes
          (arithmetic-shift reduced-int -8)
          (- num-bytes-left 1)) ) ))

  (convert-remaining-bytes
    (signed-int->twos-complement int pad-to-total-bytes)
    pad-to-total-bytes))

(module-export
  int->little-endian)
