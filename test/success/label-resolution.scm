(require "../../recursive/byte-utils")
(require "../../recursive/label-resolution")

;; UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (resolve-and-print insts)
  (display (label-resolution:resolve-local-labels insts)) (newline))

;; This function is defined in label-resolution.scm, but it's not exported. In
;; the future, maybe the function can be moved out to a common location? Until
;; then, just copy the implementation here.
(define (repeat-value val times)
  (define (helper so-far times-left)
    (if (= times-left 0)
      so-far
      (helper (cons val so-far) (- times-left 1)) ))

  (helper '() times))

(define NOP (label-resolution:bytes '(0x90)))
(define (jmp delta)
  (if (and (> delta -129)
           (< delta  128))
    (cons 0xeb (byte-utils:int->little-endian delta 1))
    (cons 0xe9 (byte-utils:int->little-endian delta 4)) ))

;; MAIN TEST ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(display "
  label1:
    nop
    jmp label2
    nop
    jmp label1
    nop
  label2:
\n")

(resolve-and-print
  (list
    (label-resolution:label-def 'label1)
    NOP
    (label-resolution:label-ref jmp '(label2))
    NOP
    (label-resolution:label-ref jmp '(label1))
    NOP
    (label-resolution:label-def 'label2)) )

(display "

--------------------------------------------------------------------------------\n

  label1:
    nop
    jmp label2
    nop (× 123)
    jmp label1
    nop
  label2:
\n")

(resolve-and-print
  (append
    (list (label-resolution:label-def 'label1))
    (list NOP)
    (list (label-resolution:label-ref jmp '(label2)))
    (repeat-value NOP 123)
    (list (label-resolution:label-ref jmp '(label1)))
    (list NOP)
    (list (label-resolution:label-def 'label2))) )

(display "

--------------------------------------------------------------------------------\n

  label1:
    nop
    jmp label2
    nop (× 124)
    jmp label1
    nop
  label2:
\n")

(resolve-and-print
  (append
    (list (label-resolution:label-def 'label1))
    (list NOP)
    (list (label-resolution:label-ref jmp '(label2)))
    (repeat-value NOP 124)
    (list (label-resolution:label-ref jmp '(label1)))
    (list NOP)
    (list (label-resolution:label-def 'label2))) )
