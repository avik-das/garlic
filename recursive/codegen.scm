(require assoc)
(require file)

(require "ast")
(require "byte-utils" *)
(require "compiler-error" => err)
(require "elf-x86-64-linux-gnu" => elf)
(require "label-resolution")
(require "location" => loc)
(require "result")

;; CODEGEN DATA STRUCTURE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (cg-new code data) (list code data))
(define (cg-empty) (cg-new '() (assoc:empty)))
(define (cg-code code) (cg-new code (assoc:empty)))
(define cg-code-single (compose cg-code list))

(define cg-get-code car)
(define cg-get-data (compose car cdr))

(define (cg-append-code cg added-code)
  (let (((old-code data) cg))
    (cg-new
      (append old-code added-code)
      data) ))

(define (cg-append-data cg added-data-key added-data-value)
  (let (((code old-data) cg))
    (cg-new
      code
      (assoc:add old-data added-data-key added-data-value)) ))

(define (cg-map-code f cg)
  (let (((old-code data) cg))
    (cg-new
      (f old-code)
      data) ))

(define (cg-merge cg1 cg2)
  (let (((code1 data1) cg1)
        ((code2 data2) cg2))
    (cg-new
      (append code1 code2)
      (assoc:merge data1 data2)) ))

(define (cg-merge-all cgs)
  (reduce cg-merge (cg-empty) cgs))

(define (cg-resolve-labels partial-cg)
  (cg-map-code label-resolution:resolve-local-labels partial-cg))

;; OPCODES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (fits-in-sint8 int)
  (and (> int -129)
       (< int  128)) )

(define (int->uint8  int) (byte-utils:int->little-endian int 1))
(define (int->uint32 int) (byte-utils:int->little-endian int 4))

; TODO: specify register somehow
(define (opcode-mov-imm32 val)
  (append
    '(0x48 0xc7 0xc0)             ; mov <immediate>, %rax
    (int->little-endian val 4) )) ;     <immediate>

; TODO: specify register somehow
(define (opcode-cmp-imm8 val)
  (list 0x48 0x83 0xf8 val)) ; cmp <immediate>, %rax

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
    (cg-code
      (opcode-mov-imm32
        ; Integers are represented as tagged pointers:
        ;
        ;   val = (raw << 1) | 1
        (bitwise-ior
          (arithmetic-shift (ast:int-get-value int) 1)
          1))) ))

(define (codegen-bool bool)
  (result:new-success
    (cg-code
      ; Booleans are represented as tagged pointers
      (opcode-mov-imm32 (if (ast:bool-get-value bool) 2 4))) ))

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

  (define (state-new cg index) (list cg index))
  (define (initial-state) (state-new (cg-empty) 0))
  (define (state->next state added-cg)
    (state-new
      (cg-merge (state-get-cg state) added-cg)
      (+ (state-get-index state) 1)) )
  (define state-get-cg car)
  (define state-get-index (compose car cdr))

  (define code-bytes-to-partial-cg-bytes (compose list label-resolution:bytes))

  (define (partial-codegen-clause index max-index clause)
    (let* ((expr-cond (ast:conditional-clause-get-condition clause))
           (result-cond (codegen-statement-list (list expr-cond)))

           (expr-body (ast:conditional-clause-get-body-statements clause))
           (result-body (codegen-statement-list expr-body))

           (result-combined
             (result:combine-results (list result-cond result-body))))
      (result:transform-success
        (lambda (cond-and-body-cgs)
          (result:new-success
            (cg-merge-all
              (list
                (cg-code-single (label-resolution:label-def index))

                ; Evaluate condition
                (cg-map-code
                  code-bytes-to-partial-cg-bytes
                  (car cond-and-body-cgs))

                ; If condition is false, jump to next clause
                (cg-code
                  (list
                    (label-resolution:bytes (opcode-cmp-imm8 4))
                    (opcode-je-to-label (+ index 1))))

                ; Otherwise (condition is true):
                ;
                ; Execute body
                (cg-map-code
                  code-bytes-to-partial-cg-bytes
                  (car (cdr cond-and-body-cgs)))

                ; And if needed, jump (skip) all the way to the end
                (cg-code-single
                  (if (= index max-index)
                    (label-resolution:bytes '())
                    (opcode-jmp-to-label 'end))) ))))
        result-combined) ))

  (define (partial-codegen-else index clause)
    (let* ((expr-body (ast:conditional-else-get-body-statements clause))
           (result-body-cg (codegen-statement-list expr-body)))
      (result:transform-success
        (lambda (body-cg)
          (result:new-success
            (cg-merge
              (cg-code-single (label-resolution:label-def index))
              (cg-map-code code-bytes-to-partial-cg-bytes body-cg) )))
        result-body-cg) ))

  (define (partial-codegen)
    (let* ((clauses (ast:conditional-get-clauses conditional))
           (num-clauses (length clauses))
           (max-index (- num-clauses 1)))
      (define (reducer maybe-state clause)
        (define (add-cg-to-state state added-cg)
          (result:new-success
            (state->next (result:get-value maybe-state) added-cg)))

        (if (result:is-error? maybe-state)
          maybe-state

          (let* ((state (result:get-value maybe-state))
                 (add-cg (lambda (cg) (add-cg-to-state state cg))))
            (if (ast:conditional-clause? clause)
              (result:transform-success
                add-cg
                (partial-codegen-clause
                  (state-get-index state)
                  max-index
                  clause))

              ; else clause
              (result:transform-success
                add-cg
                (partial-codegen-else (state-get-index state) clause)) )) ))

      (result:transform-success
        (lambda (reduced-state)
          (result:new-success
            (cg-append-code
              (state-get-cg reduced-state)
              (list (label-resolution:label-def 'end)) ) ))
        (reduce reducer (result:new-success (initial-state)) clauses)) ))

  (let* ((partial-cg (partial-codegen)))
    (result:transform-success
      (compose result:new-success cg-resolve-labels)
      partial-cg) ))

(define (codegen-quoted-list list-expression)
  (let ((ls (ast:quoted-list-get-list list-expression)))
    (if (null? ls)
      (result:new-success
        (cg-code '(0x48 0xc7 0xc0 0x00 0x00 0x00 0x00))) ; mov $0, %rax

      ; TODO: non-nil list support requires a runtime with "cons" functionality
      (result:new-with-single-error
        (err:new
          (ast:get-location list-expression)
          "Non-nil lists not yet supported by codegen")) )))

(define (codegen-statement-list statements)
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
    (compose result:new-success cg-merge-all)
    (result:combine-results
      (map codegen-statement statements))) )

(define (codegen-module module)
  (let ((cg-result
          (codegen-statement-list (ast:module-get-statements module))) )
    (result:transform-success
      (lambda (cg)
        (result:new-success
          (cg-append-code
            cg
            '(0x48 0x89 0xc7                     ; mov  %rax, %rdi
              0x48 0xc7 0xc0 0x3c 0x00 0x00 0x00 ; mov  $60, %rax
              0x0f 0x05))))                      ; syscall
        cg-result) ))

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
  (let ((cg-result (codegen-module module)))
    (result:transform-success
      (lambda (cg)
        ((compose
           (lambda (_) (result:new-success '()))
           ; Note: the file module doesn't indicate errors using the "result"
           ; type, choosing instead to print the error and exit the program.
           ; Thus, if the execution proceeds to the next step, then the write
           ; must have been successful.
           (lambda (b) (file:write-bytes filename b))
           (lambda (e) (elf:emit-as-bytes e))
           (lambda (e) (elf:add-executable-code e 'main (cg-get-code cg))))
         (elf:empty-static-executable)))
      cg-result) ))

(module-export
  write-executable-elf-from-module)
