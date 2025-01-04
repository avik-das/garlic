(require assoc)

(require "byte-utils")

;; UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (repeat-value val times)
  (define (helper so-far times-left)
    (if (= times-left 0)
      so-far
      (helper (cons val so-far) (- times-left 1)) ))

  (helper '() times))

;; DATA STRUCTURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (label-def name) (list 'label-def name))
(define label-def-get-name (compose car cdr))

(define (label-ref generator labels) (list 'label-ref generator labels))
(define (label-ref->initial-attempt ref)
  (let (((tag generator labels) ref))
    (label-ref-attempt
      generator
      labels
      (apply generator (repeat-value 0 (length labels)))) ))

(define (bytes bs) (list 'bytes bs))
(define bytes-get-bytes (compose car cdr))

(define (label-ref-attempt generator labels bytes)
  (list 'label-ref-attempt generator labels bytes))
(define label-ref-attempt-get-labels (compose car cdr cdr))
(define label-ref-attempt-get-bytes (compose car cdr cdr cdr))
(define (label-ref-attempt->next-attempt ref deltas)
  (let (((tag generator labels . rest) ref))
    (label-ref-attempt
      generator
      labels
      (apply generator deltas)) ))

(define (starts-with-symbol? ast symbol)
  (and (list? ast)
       (symbol? (car ast))
       (= (car ast) symbol)))

(define (type-checker type)
  (lambda (ast) (starts-with-symbol? ast type)) )

(define label-def? (type-checker 'label-def))
(define label-ref? (type-checker 'label-ref))
(define label-ref-attempt? (type-checker 'label-ref-attempt))
(define bytes? (type-checker 'bytes))

(define (instruction-get-size inst)
  (cond
    ((label-def? inst) 0)
    ((label-ref-attempt? inst) (length (label-ref-attempt-get-bytes inst)))
    ((bytes? inst) (length (bytes-get-bytes inst)))
    (else (error-and-exit "Unknown instruction: " inst)) ))

;; MAIN LOGIC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (instructions-to-initial-attempts insts)
  (define (to-initial-attempt inst)
    (cond
      ((label-def? inst) inst)
      ((bytes? inst) inst)
      ((label-ref? inst) (label-ref->initial-attempt inst))
      (else (error-and-exit "Unknown instruction: " inst)) ))

  (map to-initial-attempt insts))

(define (compute-label-addrs insts)
  (define (state-new addrs curr-addr) (list addrs curr-addr))
  (define (initial-state) (state-new (assoc:empty) 0))

  (define state-get-addrs car)

  (define (reducer state inst)
    (let (((addrs curr-addr) state))
      (if (label-def? inst)
        (let ((label (label-def-get-name inst)))
          (if (assoc:has-key? addrs label)
            (error-and-exit "Duplicate label: " label)
            (state-new (assoc:add addrs label curr-addr) curr-addr)))

        (state-new addrs (+ curr-addr (instruction-get-size inst))) )))

  (state-get-addrs (reduce reducer (initial-state) insts)))


(define (iteration-state-new addr has-changed processed-insts)
  (list addr has-changed processed-insts))
(define (initial-iteration-state) (iteration-state-new 0 #f '()))

(define iteration-state-get-curr-addr car)
(define iteration-state-get-has-changed (compose car cdr))
(define iteration-state-get-instructions (compose car cdr cdr))

(define (attempt-once insts)
  (define (list=? l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((null? l1) #f)
      ((null? l2) #f)
      (else
        (and (= (car l1) (car l2))
             (list=? (cdr l1) (cdr l2)))) ))

  (define (iteration-state-evolve-with-attempt state inst prev-inst)
    (let (((addr has-changed processed-insts) state))
      (iteration-state-new
        (+ addr (instruction-get-size inst))
        (or has-changed
            (not (list=? (label-ref-attempt-get-bytes prev-inst)
                         (label-ref-attempt-get-bytes inst))))
        (append processed-insts (list inst))) ))

  (define (iteration-state-evolve-static state inst)
    (let (((addr has-changed processed-insts) state))
      (iteration-state-new
        (+ addr (instruction-get-size inst))
        has-changed
        (append processed-insts (list inst))) ))

  (define (process-instruction label-addrs state inst)
    (define (label-ref-attempt-deltas curr-addr inst)
      (define (compute-delta curr-addr label)
        (- (assoc:get label-addrs label)
           (+ curr-addr (instruction-get-size inst))) )

      (map
        (lambda (label) (compute-delta curr-addr label))
        (label-ref-attempt-get-labels inst)) )

    (cond
      ((label-def? inst) (iteration-state-evolve-static state inst))
      ((bytes? inst) (iteration-state-evolve-static state inst))

      ((label-ref-attempt? inst)
       (iteration-state-evolve-with-attempt
         state
         (label-ref-attempt->next-attempt
           inst
           (label-ref-attempt-deltas
             (iteration-state-get-curr-addr state) inst))
         inst)) ))

  (let ((label-addrs (compute-label-addrs insts)))
    (reduce
      (lambda (state inst) (process-instruction label-addrs state inst))
      (initial-iteration-state)
      insts) ))

(define (iterate-until-resolved insts max-iterations)
  (define (iterate insts num-iterations)
    (if (= num-iterations 0)
      insts

      (let* ((new-state (attempt-once insts))
             (has-changed (iteration-state-get-has-changed new-state))
             (new-insts (iteration-state-get-instructions new-state)))
        (if has-changed
          (iterate new-insts (- num-iterations 1))
          new-insts) )))

  (iterate insts max-iterations))

(define (instructions->bytes insts)
  (define (inst->bytes inst)
    (cond
      ((label-def? inst) '())
      ((bytes? inst) (bytes-get-bytes inst))
      ((label-ref-attempt? inst) (label-ref-attempt-get-bytes inst))
      (else (error-and-exit "Unknown instruction: " inst)) ))

  ; TODO: should actually flatten the list, but right now, I'm keeping the bytes
  ;   in chunks for easier printing
  (reduce
    append
    '()
    (map inst->bytes insts)) )

;;   (list
;;     (bytes '(0x01 0x02 0x03 0x03))
;;     (label-ref (list 'label-else) ...)  ; ---+
;;     0x04 0x05 0x06 0x07                 ;    |
;;     (label-ref (list 'label-end) ...)   ; ---|--+
;;     (label-def 'label-else)             ; <--+  |
;;     (bytes '(0x08 0x09 0x0a 0x0b))      ;       |
;;     (label-def 'label-end)              ; <-----+
;;
(define (resolve-local-labels insts)
  ; 1. Convert instructions to attempts
  ; 2. Perform 10 attempts, breaking as soon as stable
  ;    1. Compute label addresses
  ;    2. Generate attempts based on addresses
  ; 3. Convert attempts to bytes

  (let* ((initial-attempt (instructions-to-initial-attempts insts))
         (resolved (iterate-until-resolved initial-attempt 10))
         (bytes (instructions->bytes resolved)))
    bytes))

(module-export
  label-ref
  label-def
  bytes

  resolve-local-labels)
