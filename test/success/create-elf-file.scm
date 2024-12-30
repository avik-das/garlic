(require "../../recursive/elf-x86-64-linux-gnu" => elf)
(require "../aux/hexdump")

(define test-code
  '(0x48 0xc7 0xc0 0x3c 0x00 0x00 0x00 ; mov  $60, %rax
    0xbf 0x2a 0x00 0x00 0x00           ; mov  $42, %edi
    0x0f 0x05))                        ; syscall

((compose
   (lambda (b) (hexdump:print-bytes b))
   (lambda (e) (elf:emit-as-bytes e))
   (lambda (e) (elf:add-executable-code e 'main test-code)))
 (elf:empty-static-executable))
