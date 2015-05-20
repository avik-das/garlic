(require string => str)

(define (tag-fn name nl)
  (lambda args
    (if (null? args)
      (str:concat "<" name " />")
      (format-tag-with-args name nl args))) )

(define (gather-attrs args)
  (define (add-attr attr-name remaining)
    (if (null? remaining)
      '() ; should not happen
      (cons (cons attr-name (car remaining)) (gather-attrs (cdr remaining))) ))

  (cond
    ((null? args) '())
    ((symbol? (car args)) (add-attr (car args) (cdr args)))
    (else (gather-attrs (cdr args)))) )

(define (tag-children args)
  (define (remove-attr remaining)
    (let* ((first (car remaining))
           (len-rest (length (cdr remaining))) )
      (if (symbol? first)
        (if (= len-rest 0)
          '()
          (tag-children (cdr (cdr remaining))) )
        (cons first (tag-children (cdr remaining))) ) ))

  (if (null? args)
    '()
    (remove-attr args)) )

(define (format-tag-with-args name nl args)
  (define (format-attr attr)
    (str:concat
      (str:symbol->str (car attr))
      "=\""
      (cdr attr)
      "\""))

  (define (format-attrs attrs)
     (map format-attr attrs))

  (let ((attrs (gather-attrs args))
        (children (tag-children args))
        (sep (if nl "\n" "")))
    (str:concat
      "<"
      name
      (if (null? attrs) "" " ")
      (str:concat-list (format-attrs attrs))
      ">"
      sep
      (str:concat-list children)
      sep
      "</"
      name
      ">")) )

(define html  (tag-fn "html" #t))
(define title (tag-fn "title" #f))
(define body  (tag-fn "body" #t))
(define p     (tag-fn "p" #f))
(define a     (tag-fn "a" #f))
(define img   (tag-fn "img" #f))

(define (head title-text . children)
  (format-tag-with-args "head" #t (cons (title title-text) children)) )

(module-export
  html
  head
  body
  p
  a
  img)
