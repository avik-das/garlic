(require assoc)

(require "../../recursive/elf-x86-64-linux-gnu" => elf)
(require "../aux/hexdump")

(define test-code-base
  '(0x48 0xc7 0xc0 0x3c 0x00 0x00 0x00 ; mov  $60, %rax
    0xbf 0x2a 0x00 0x00 0x00           ; mov  $42, %edi
    0x0f 0x05))                        ; syscall

((compose
   (lambda (b) (hexdump:print-bytes b))
   (lambda (e) (elf:emit-as-bytes e))
   (lambda (e) (elf:add-executable-code e 'main test-code-base)))
 (elf:empty-static-executable))

(newline)

(define test-code-referencing-data
  (list
    0xb8 0x01 0x00 0x00 0x00   ; mov  $1, %eax
    0xbf 0x01 0x00 0x00 0x00   ; mov  $1, %edi
    0xbe (elf:data-ref 'msg 4) ; mov  $msg, %esi -- will be resolved to address
    0xba 0x0e 0x00 0x00 0x00   ; mov  $14, %edx
    0x0f 0x05                  ; syscall

    0xb8 0x3c 0x00 0x00 0x00   ; mov  $60, %eax
    0xbf 0x2a 0x00 0x00 0x00   ; mov  $42, %edi
    0x0f 0x05))                ; syscall

(define test-data
  (assoc:singleton
    'msg
    '(0x48 0x65 0x6c 0x6c 0x6f 0x2c 0x20    ; "Hello, "
      0x77 0x6f 0x72 0x6c 0x64 0x21 0x0a))) ; "world!\n"

((compose
   (lambda (b) (hexdump:print-bytes b))
   (lambda (e) (elf:emit-as-bytes e))
   (lambda (e) (elf:add-data e test-data))
   (lambda (e) (elf:add-executable-code e 'main test-code-referencing-data)))
 (elf:empty-static-executable))
