(require string => str)
(require file)

(require "location" => loc)
(require "result")

(require "lexer")
(require "parser")
(require "ast")
(require "evaluator")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (= (length *argv*) 2))
  (begin
    (display "USAGE: " (car *argv*) " <filename>")
    (newline))
  '())

(define (show-errors errs)
  (define (show-single-error err)
    ; TODO: show snippet from original file
    (let ((loc (lexer:lex-error-get-location err))
          (msg (lexer:lex-error-get-message err)))
      (display
        "  "
        "ERROR: "
        msg
        " ("
        (loc:get-filename loc)
        ":"
        (loc:get-line loc)
        ":"
        (loc:get-column loc)
        ")")
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

(define filename (car (cdr *argv*)))
(define input (file:read-text filename) )
(define lexed (lexer:lex filename input))

(if (result:is-error? lexed)
    (begin
      (show-errors (result:get-errors lexed))
      (newline)
      (error-and-exit "COMPILATION FAILED"))
    '() ) ; Otherwise: continue

(define parsed (parser:parse (result:get-value lexed)))
(evaluator:eval-module parsed)
