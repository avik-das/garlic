(require "../../recursive/byte-utils" => bu)

(define (test-int->little-endian num-bytes ints)
  (display "num-bytes: " num-bytes) (newline)
  (display "------------") (newline)

  (foreach
    (lambda (int)
      (display "  " int " -> " (bu:int->little-endian int num-bytes))
      (newline))
    ints))

;; 1 byte
(test-int->little-endian
  1
  '(   0
       1
       2
       4
       8
      16
      32
      64
     127
      -1
      -2
      -4
      -8
     -16
     -32
     -64
    -127
    -128))
(newline)

;; 2 bytes
(test-int->little-endian
  2
  '(     0
         1
         2
         4
         8
        16
        32
        64
       128
       256
       512
      1024
     32767
        -1
        -2
        -4
        -8
       -16
       -32
       -64
      -128
      -256
      -512
     -1024
    -32767
    -32768))
(newline)

;; 4 bytes
(test-int->little-endian
  4
  '(
              0
              1
              2
              4
              8
             16
             32
             64
            128
            256
            512
           1024
          32767
     2147483647
             -1
             -2
             -4
             -8
            -16
            -32
            -64
           -128
           -256
           -512
          -1024
         -32767
         -32768
    -2147483647
    -2147483648))
(newline)
