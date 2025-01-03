(require file)

(require "ast")
(require "byte-utils" *)
(require "compiler-error" => err)
(require "elf-x86-64-linux-gnu" => elf)
(require "label-resolution")
(require "location" => loc)
(require "result")

;; OPCODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fits-in-sint8 int)
  (and (> int -129)
       (< int  128)) )

(define (int->uint8  int) (byte-utils:int->little-endian int 1))
(define (int->uint32 int) (byte-utils:int->little-endian int 4))

(define (opcode-je-to-label label)
  (define (generator delta)
    (if (fits-in-sint8 delta)
      (cons          0x74  (int->uint8  delta))
      (append '(0x0f 0x84) (int->uint32 delta)) ))

  (label-resolution:label-ref generator (list label)) )

(define (opcode-jmp-to-label label)
  (define (generator delta)
    (if (fits-in-sint8 delta)
      (cons 0xeb (int->uint8  delta))
      (cons 0xe9 (int->uint32 delta)) ))

  (label-resolution:label-ref generator (list label)) )

;; AST CODEGEN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (codegen-int int)
  (result:new-success
    (append
      '(0x48 0xc7 0xc0)   ; mov <immediate>, %rax
      (int->little-endian ;     <immediate>
        (ast:int-get-value int) ; TODO: convert to fixnum representation
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
  ;   label_1:
  ;     <code for cond-1>
  ;     cmp  $4, %rax
  ;     je   label_2
  ;     <code for body-1>
  ;     jmp  label_end
  ;   label_2:
  ;     <code for cond-2>
  ;     cmp  $4, %rax
  ;     je   label_3
  ;     <code for body-2>
  ;     jmp  label_end
  ;   label_3:
  ;     <code for cond-3>
  ;     cmp  $4, %rax
  ;     je   label_4
  ;     <code for body-3>
  ;     jmp  label_end
  ;   label_4:
  ;     <code for body-else>
  ;   label_end:

  (define (state-new insts index) (list insts index))
  (define (initial-state) (state-new '() 0))
  (define (state->next state added-insts)
    (state-new
      (append (state-get-insts state) added-insts)
      (+ (state-get-index state) 1)) )
  (define state-get-insts car)
  (define state-get-index (compose car cdr))

  (define (partial-codegen-clause index max-index clause)
    (let* ((expr-cond (ast:conditional-clause-get-condition clause))
           (result-cond (codegen-statement-list (list expr-cond)))

           (expr-body (ast:conditional-clause-get-body-statements clause))
           (result-body (codegen-statement-list expr-body))

           (result-combined
             (result:combine-results (list result-cond result-body))))
      (result:transform-success
        (lambda (cond-and-body)
          (result:new-success
            (list
              (label-resolution:label-def index)

              ; Evaluate condition
              (label-resolution:bytes (car cond-and-body))

              ; If condition is false, jump to next clause
              (label-resolution:bytes '(0x48 0x83 0xf8 0x04)) ; cmp $4, %rax
              (opcode-je-to-label (+ index 1))

              ; Otherwise (condition is true):
              ;
              ; Execute body
              (label-resolution:bytes (car (cdr cond-and-body)))

              ; And if needed, jump (skip) all the way to the end
              (if (= index max-index)
                (label-resolution:bytes '())
                (opcode-jmp-to-label 'end)) )))
        result-combined) ))

  (define (partial-codegen-else index clause)
    (let* ((expr-body (ast:conditional-else-get-body-statements clause))
           (result-body (codegen-statement-list expr-body)))
      (result:transform-success
        (lambda (body)
          (result:new-success
            (list
              (label-resolution:label-def index)
              (label-resolution:bytes body) )))
        result-body) ))

  (define (partial-codegen)
    (let* ((clauses (ast:conditional-get-clauses conditional))
           (num-clauses (length clauses))
           (max-index (- num-clauses 1)))
      (define (reducer maybe-state clause)
        (define (add-insts-to-state state added-insts)
          (result:new-success
            (state->next (result:get-value maybe-state) added-insts)))

        (if (result:is-error? maybe-state)
          maybe-state

          (let* ((state (result:get-value maybe-state))
                 (add-insts (lambda (insts) (add-insts-to-state state insts))))
            (if (ast:conditional-clause? clause)
              (result:transform-success
                add-insts
                (partial-codegen-clause
                  (state-get-index state)
                  max-index
                  clause))

              ; else clause
              (result:transform-success
                add-insts
                (partial-codegen-else (state-get-index state) clause)) )) ))

      (result:transform-success
        (lambda (reduced-state)
          (result:new-success
            (append
              (state-get-insts reduced-state)
              (list (label-resolution:label-def 'end)) ) ))
        (reduce reducer (result:new-success (initial-state)) clauses)) ))

  (let* ((insts (partial-codegen)))
    (result:transform-success
      (lambda (insts)
        (result:new-success
          (label-resolution:resolve-local-labels insts)))
      insts) ))

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

;; MAIN LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
