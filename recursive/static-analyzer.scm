(require set)
(require string => str)

(require "ast")
(require "compiler-error" => err)
(require "result")

;; Perform static analysis on the given module, returning a "result" with
;; either all the errors that were found, or the modified module AST. Examples
;; of both these scenarios are:
;;
;; 1. Errors can occur when a variable references an undefined name.
;; 2. In the case of a success, "define" statements are hoisted to the top of
;;    their respective scopes so they can execute before any other statements
;;    that reference them.
(define (analyze-module module)
  ; These names are defined in every module. In the Ruby implementation, these
  ; names are handled by the fact that every module implicitly includes some
  ; "core" modules that define these names, but since module support is not yet
  ; implemented in the recursive compiler, for now define these names
  ; explicitly here in the static analyzer.
  ;
  ; Keep this in sync with the functions defined in the evaluator.
  ;
  ; The goal is to eventually remove these altogether (though based on the Ruby
  ; implementation, there is still *argv* that needs to be handled this way).
  (define toplevel-names
    (set:new-with-equality
      str:string=?
      (list
        "="
        "+"
        "-"
        "*"
        "cons"
        "car"
        "cdr"
        "null?"
        "display"
        "newline")))

  (result:transform-success
    (lambda (analyzed-statements)
      (result:new-success (ast:module analyzed-statements)))
    (analyze-statement-list
      (ast:module-get-statements module)
      toplevel-names)) )

(define (analyze-statement-list statements names-in-scope)
  (define (separate-defines statements)
    (let ((defines (filter ast:definition? statements))
          (non-defines (reject ast:definition? statements)))
      (list defines non-defines)))

  (let* (((defines non-defines) (separate-defines statements))
         (reordered-statements (append defines non-defines))
         (defined-names (map ast:definition-get-name defines))
         (new-names-in-scope (set:add-all names-in-scope defined-names)))
    (result:combine-results
      (map
        (lambda (statement) (analyze-ast-node statement new-names-in-scope))
        reordered-statements)) ))

(define (analyze-ast-node node names-in-scope)
  (cond
    ; The following nodes are self-identifying, meaning they don't reference
    ; any names in the scope. Thus, they always succeed the analysis.
    ((or (ast:int? node)
         (ast:atom? node)
         (ast:bool? node)
         (ast:str? node)
         (ast:quoted-list? node))
     (result:new-success node))

    ; A variable reference simply needs to check that the referenced name
    ; exists in the current scope.
    ((ast:var? node)
     (let ((name (ast:var-get-name node)))
       (if (set:contains? names-in-scope name)
           (result:new-success node)
           (result:new-with-single-error
             (err:new
               (ast:get-location node)
               "undefined variable '" name  "'")) )))

    ; A definition does define a new name (by definition!), but that name is
    ; already added to the current scope by "analyze-statement-list".
    ((ast:definition? node)
     (result:transform-success
       (lambda (analyzed)
         (result:new-success
           (ast:definition (ast:definition-get-name node) analyzed)))
       (analyze-ast-node (ast:definition-get-body node) names-in-scope) ) )

    ; A conditional does not define any new names
    ((ast:conditional? node)
     (result:transform-success
       (compose result:new-success ast:conditional)
       (result:combine-results
         (map
           (lambda (node) (analyze-ast-node node names-in-scope))
           (ast:conditional-get-clauses node)) ) ) )

    ; A conditional clause does not define any new names
    ((ast:conditional-clause? node)
     (result:transform-success
       (lambda (analyzed)
         (result:new-success
           (ast:conditional-clause (car analyzed) (car (cdr analyzed)))))
       (result:combine-results
         (list
           (analyze-ast-node
             (ast:conditional-clause-get-condition node)
             names-in-scope)
           (analyze-statement-list
             (ast:conditional-clause-get-body-statements node)
             names-in-scope)) ) ) )

    ; A conditional else does not define any new names
    ((ast:conditional-else? node)
     (result:transform-success
       (compose result:new-success ast:conditional-else)
       (analyze-statement-list
         (ast:conditional-else-get-body-statements node)
         names-in-scope) ) )

    ; A function defines new names based on its parameter list
    ((ast:function? node)
     (let* ((args (ast:function-get-args node))
            (new-names-in-scope (set:add-all names-in-scope args)))
       (result:transform-success
         (lambda (analyzed)
           (result:new-success (ast:function args analyzed)))
         (analyze-statement-list
           (ast:function-get-body node)
           new-names-in-scope) ) ))

    ; A function call does not define any new names
    ((ast:function-call? node)
     (result:transform-success
       (lambda (analyzed)
         (result:new-success
           (ast:function-call (car analyzed) (cdr analyzed))))
       (result:combine-results
         (map
           (lambda (node) (analyze-ast-node node names-in-scope))
           (cons
             (ast:function-call-get-function node)
             (ast:function-call-get-args node))) ) ) )

    (else
      (display "analyze-ast-node: unhandled node '" node "'")
      (newline)
      (result:new-success node)) ))

(module-export
  analyze-module)
