(require file)

(require "ast")
(require "byte-utils" *)
(require "compiler-error" => err)
(require "elf-x86-64-linux-gnu" => elf)
(require "location" => loc)
(require "result")

(define (codegen-int-statement int)
  (append
    '(0x48 0xc7 0xc0)   ; mov <immediate>, %rax
    (int->little-endian ;     <immediate>
      (ast:int-get-value int)
      4)) )

(define (codegen-statement-list statements)
  (define (codegen-ints int-statements)
    (if (null? int-statements)
      '()
      (append
        (codegen-int-statement (car int-statements))
        (codegen-ints (cdr int-statements))) ))

  (let ((int-statements (filter ast:int? statements))
        (non-int-statements (reject ast:int? statements)))
    (if (not (null? non-int-statements))
      ; TODO: present an error for each non-int statement, extracting the
      ;   correct location for each statment. Right now, only `var`s have
      ;   location information attached. In the future, all nodes should have
      ;   locations attached!
      ;
      ;   Note that because the location is chosen as the start of the file, the
      ;   error display may show the wrong code
      (result:new-with-single-error
        (err:new
          (loc:start "<input>")
          "Non-int statements not yet supported"))
      (result:new-success (codegen-ints int-statements))) ))

(define (codegen-module module)
  (let ((code-result
          (codegen-statement-list (ast:module-get-statements module))) )
    (result:transform-success
      (lambda (code)
        (result:new-success
          (append
            code
            '(0x48 0x89 0xc7                     ; mov  %rax, %rdi
              0x48 0xc7 0xc0 0x3c 0x00 0x00 0x00 ; mov  $60, %rax
              0x0f 0x05))))                      ; syscall
        code-result) ))

;; Generate an ELF file corresponding to the given module, returning a "result"
;; with either all the errors that were found, or an empty successful result
;; indicating that the file was successfully written.
;;
;; The generated ELF file will be a standalone executable, as opposed to a
;; library. In particular, that means there is an entrypoint pointing to the
;; `.text` section that is executed when the ELF file is run.
;;
;; @param filename - the filename to write to
;; @param module - the module to generate the ELF file for
(define (write-executable-elf-from-module filename module)
  (let ((code-result (codegen-module module)))
    (result:transform-success
      (lambda (code)
        ((compose
           (lambda (_) (result:new-success '()))
           ; Note: the file module doesn't indicate errors using the "result"
           ; type, choosing instead to print the error and exit the program.
           ; Thus, if the execution proceeds to the next step, then the write
           ; must have been successful.
           (lambda (b) (file:write-bytes filename b))
           (lambda (e) (elf:emit-as-bytes e))
           (lambda (e) (elf:add-executable-code e 'main code)))
         (elf:empty-static-executable)))
      code-result) ))

(module-export
  write-executable-elf-from-module)
