(require string => str)
(require file)

(require "compiler-error" => err)
(require "location" => loc)
(require "result")

(require "lexer")
(require "parser")
(require "static-analyzer")
(require "codegen")
(require "elf-x86-64-linux-gnu" => elf)
(require "evaluator")

(require "compiler_utils" => compiler-utils)

;; ARGUMENT PARSING ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parsed-args-intepreted input-filename)
  (list 'parsed-args input-filename '() #t))

(define (parsed-args-compiled input-filename output-filename)
  (list 'parsed-args input-filename output-filename #f))

(define parsed-args-get-input-filename (compose car cdr cdr))
(define parsed-args-get-output-filename (compose car cdr cdr cdr))
(define parsed-args-get-is-intepreted (compose car cdr cdr cdr cdr))

(define (print-usage-and-exit args)
  ; The first argument, corresponding to the executable name, is guaranteed to
  ; be present
  (define name (car args))

  (display "USAGE:\n")
  (newline)
  (display "  " name " [-O <output-filename>] <input-filename>\n")
  (display "  " name " --interpret <input-filename>\n")
  (display "  " name " (-h|--help)\n")
  (newline)
  (display "OPTIONS:\n")
  (newline)
  (display "  -O <output-filename>, --output <output-filename>\n")
  (display "      the output filename of the compiled executable\n")
  (display "  --interpret  run in intepreted mode\n")
  (display "  -h, --help   show this message and exit\n")
  (newline)

  (error-and-exit "COMPILATION FAILED"))

;; Parse the command line arguments.
;;
;; @param args - the unprocessed command line arguments, accessible via *argv*
;; @return a `result` of either the parsed arguments or an error. The error will
;;         simply be an empty list to indicate failure.
(define (parse-args args)
  ; A more scalable approach is to implement an actual tree-based parser, which
  ; goes through the different options and branches to different paths. That
  ; would allow for options that are, for example, shared across different
  ; subcommands. For now, keep it simple by assumpting a few specific
  ; combinations of options.
  (cond
    ; executable --interpret <input-filename>
    ((and (= (length args) 3)
          (str:string=? (car (cdr args)) "--interpret"))
     (result:new-success
       (parsed-args-intepreted (car (cdr (cdr args))))))

    ; executable <input-filename>
    ((and (= (length args) 2)
          (not (str:string=? (str:at (car (cdr args)) 0) "-")))
     (result:new-success
       (parsed-args-compiled (car (cdr args)) "main")))

    ; executable -O <output-filename> <input-filename>
    ; executable --output <output-filename> <input-filename>
    ((and (= (length args) 4)
          (or (str:string=? (car (cdr args)) "-O")
              (str:string=? (car (cdr args)) "--output"))
          (not (str:string=? (str:at (car (cdr (cdr args))) 0) "-"))
          (not (str:string=? (str:at (car (cdr (cdr (cdr args)))) 0) "-")))
     (result:new-success
       (parsed-args-compiled
         (car (cdr (cdr (cdr args))))
         (car (cdr (cdr args))))))

    ; Don't distinguish between explicit "--help" and an error
    (else (result:new-error '())) ))

;; ERROR DISPLAY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-errors input errs)
  (define (show-repeated-chars n chr)
    (if (= n 0)
        '()
        (begin
          (display chr)
          (show-repeated-chars (- n 1) chr)) ))

  (define (num-digits-for-positive-int int)
    ; Put a limit of 99,999 for the number. This is to avoid needing to
    ; implement division and rounding, or other methods of determining the
    ; number of digits in a number.
    (cond ((> int 9999) 5)
          ((> int  999) 4)
          ((> int   99) 3)
          ((> int    9) 2)
          (else 1) ))

  (define (show-single-error err)
    (let* ((loc (err:get-location err))
           (line (loc:get-line loc))
           (column (loc:get-column loc))

           (msg (err:get-message err))

           (num-chars-in-lineno (num-digits-for-positive-int line)))
      (display
        "  "
        "ERROR: "
        msg
        " ("
        (compiler-utils:filename-from-path (loc:get-filename loc))
        ":"
        line
        ":"
        column
        ")")

      (newline)
      (newline)

      (display
        "    "
        line
        "| "
        (compiler-utils:line-from-file-contents input line))

      (newline)

      (display "    ")
      (show-repeated-chars num-chars-in-lineno " ")
      (display "  ")
      (show-repeated-chars (- column 1) "-")
      (display "^")

      (newline)))

  (define (show-remaining-errors errs)
    (if (null? errs)
        '()
        (begin
          (newline)
          (show-single-error (car errs))
          (show-remaining-errors (cdr errs))) ))

  (let ((num-errors (length errs)))
    (display
      "Compilation failed ("
      num-errors
      " "
      (if (> num-errors 1) "errors" "error")
      ")")
    (newline))

  (show-remaining-errors errs))

;; MAIN LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (process-input parsed-args input)
  (result:pipeline-successes
    (result:new-success input)
    (lambda (input)
      (lexer:lex (parsed-args-get-input-filename parsed-args) input))
    (lambda (lexed) (parser:parse lexed))
    (lambda (module) (static-analyzer:analyze-module module))
    (lambda (module)
      (if (parsed-args-get-is-intepreted parsed-args)
        ; Intepret - assume success as long as the evaluator returns
        (begin
          (evaluator:eval-module module)
          (result:new-success '()))

        ; Compile
        (codegen:write-executable-elf-from-module
          (parsed-args-get-output-filename parsed-args)
          module))) ))

(define parsed-args (parse-args *argv*))
(if (result:is-error? parsed-args)
  (print-usage-and-exit *argv*)

  (let* ((input (file:read-text (parsed-args-get-input-filename parsed-args)))
         (final-result (process-input parsed-args input)))
    (if (result:is-error? final-result)
      (begin
        (show-errors input (result:get-errors final-result))
        (newline)
        (error-and-exit "COMPILATION FAILED"))
      '() ) )) ; Otherwise: continue
