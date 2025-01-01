(require file)

(require "ast")
(require "byte-utils" *)
(require "compiler-error" => err)
(require "elf-x86-64-linux-gnu" => elf)
(require "location" => loc)
(require "result")

(define (codegen-int int)
  (result:new-success
    (append
      '(0x48 0xc7 0xc0)   ; mov <immediate>, %rax
      (int->little-endian ;     <immediate>
        (ast:int-get-value int)
        4))) )

(define (codegen-bool bool)
  (result:new-success
    (append
      '(0x48 0xc7 0xc0)   ; mov <immediate>, %rax
      (int->little-endian ;     <immediate>
        ; Booleans are represented as tagged pointers
        (if (ast:bool-get-value bool) 2 4)
        4))) )

(define (codegen-conditional conditional)
  ; TODO
  ;
  ; The tricky part about the conditionals is supporting jumps to other
  ; addresses, addresses that depend on the results of codegen of the contained
  ; expressions. We might have to think about a general label-handling strategy
  ; across the board, as function calls may require labels as well.
  ;
  ; For now, return an error, but here's the code->assembly translation for
  ; reference (based on the Ruby implementation).
  ;
  ; SAMPLE GARLIC CODE:
  ;
  ;   (cond
  ;     (cond-1 body-1)
  ;     (cond-2 body-2)
  ;     (cond-3 body-3)
  ;     (else body-else))
  ;
  ; CORRESPONDING ASSEMBLY:
  ;
  ;     <code for cond-1>
  ;     cmp  $4, %rax
  ;     je   label_2
  ;     <code for body-1>
  ;     jmp  label_end
  ;   label_2:
  ;     <code for cond-2>
  ;     cmp  $4, %rax
  ;     je   label_else
  ;     <code for body-2>
  ;     jmp  label_end
  ;   label_3:
  ;     <code for cond-3>
  ;     cmp  $4, %rax
  ;     je   label_else
  ;     <code for body-3>
  ;     jmp  label_end
  ;   label_else:
  ;     <code for body-else>
  ;   label_end:

  (result:new-with-single-error
    (err:new
      (ast:get-location conditional)
      "Conditionals not yet supported by codegen")) )

(define (codegen-quoted-list list-expression)
  (let ((ls (ast:quoted-list-get-list list-expression)))
    (if (null? ls)
      (result:new-success '(0x48 0xc7 0xc0 0x00 0x00 0x00 0x00)) ; mov $0, %rax

      ; TODO: non-nil list support requires a runtime with "cons" functionality
      (result:new-with-single-error
        (err:new
          (ast:get-location list-expression)
          "Non-nil lists not yet supported by codegen")) )))

(define (codegen-statement-list statements)
  (define (concat-lists lists)
    (if (null? lists)
      '()
      (append (car lists) (concat-lists (cdr lists))) ))

  (define (codegen-statement statement)
    (cond
      ((ast:int? statement) (codegen-int statement))
      ((ast:bool? statement) (codegen-bool statement))
      ((ast:quoted-list? statement) (codegen-quoted-list statement))
      ((ast:conditional? statement) (codegen-conditional statement))
      (else
        (result:new-with-single-error
          (err:new
            (ast:get-location statement)
            "Statement not yet supported by codegen"))) ))

  (result:transform-success
    (compose result:new-success concat-lists)
    (result:combine-results
      (map codegen-statement statements))) )

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
