;;   (list
;;     0x01 0x02 0x03 0x03
;;     (list 'label-else (lambda (delta) ...))  ; ---+
;;     0x04 0x05 0x06 0x07                      ;    |
;;     (list 'label-end (lambda (delta) ...))   ; ---|--+
;;     'label-else                              ; <--+  |
;;     0x08 0x09 0x0a 0x0b                      ;       |
;;     'label-end                               ; <-----+
;;
(define (resolve-local-labels code-bytes)
  '())
