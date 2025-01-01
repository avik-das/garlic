;; An in-memory representation and associated functions for the ELF file format.
;; The scope of this module is reduced by assuming system configuration such as
;; x86-64 little-endian systems.

(require file)
(require string => str)
(require "byte-utils" *)

;; UTILITIES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (ascii-char-to-byte chr)
  (cond
    ((str:string=? chr ".")  46)
    ((str:string=? chr "A")  65)
    ((str:string=? chr "B")  66)
    ((str:string=? chr "C")  67)
    ((str:string=? chr "D")  68)
    ((str:string=? chr "E")  69)
    ((str:string=? chr "F")  70)
    ((str:string=? chr "G")  71)
    ((str:string=? chr "H")  72)
    ((str:string=? chr "I")  73)
    ((str:string=? chr "J")  74)
    ((str:string=? chr "K")  75)
    ((str:string=? chr "L")  76)
    ((str:string=? chr "M")  77)
    ((str:string=? chr "N")  78)
    ((str:string=? chr "O")  79)
    ((str:string=? chr "P")  80)
    ((str:string=? chr "Q")  81)
    ((str:string=? chr "R")  82)
    ((str:string=? chr "S")  83)
    ((str:string=? chr "T")  84)
    ((str:string=? chr "U")  85)
    ((str:string=? chr "V")  86)
    ((str:string=? chr "W")  87)
    ((str:string=? chr "X")  88)
    ((str:string=? chr "Y")  89)
    ((str:string=? chr "Z")  90)
    ((str:string=? chr "a")  97)
    ((str:string=? chr "b")  98)
    ((str:string=? chr "c")  99)
    ((str:string=? chr "d") 100)
    ((str:string=? chr "e") 101)
    ((str:string=? chr "f") 102)
    ((str:string=? chr "g") 103)
    ((str:string=? chr "h") 104)
    ((str:string=? chr "i") 105)
    ((str:string=? chr "j") 106)
    ((str:string=? chr "k") 107)
    ((str:string=? chr "l") 108)
    ((str:string=? chr "m") 109)
    ((str:string=? chr "n") 110)
    ((str:string=? chr "o") 111)
    ((str:string=? chr "p") 112)
    ((str:string=? chr "q") 113)
    ((str:string=? chr "r") 114)
    ((str:string=? chr "s") 115)
    ((str:string=? chr "t") 116)
    ((str:string=? chr "u") 117)
    ((str:string=? chr "v") 118)
    ((str:string=? chr "w") 119)
    ((str:string=? chr "x") 120)
    ((str:string=? chr "y") 121)
    ((str:string=? chr "z") 122)
    (else (error-and-exit "Cannot convert ASCII character to byte: " chr)) ))

;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (empty-static-executable)
  (new-elf '()))

(define (new-elf stubs)
  (list 'elf stubs))

;; BUILDER FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-stubs elf stubs-to-add)
  (let* ((existing-stubs (elf->stubs elf))
         (updated-stubs
           (append existing-stubs stubs-to-add)))
    (new-elf updated-stubs)))

(define (add-stub elf stub-to-add)
  (add-stubs elf (list stub-to-add)))

;; Add the given machine code (represented as an opaque list of bytes) into the
;; ELF file as the main code that will be executed when the ELF file is run.
;; This entails:
;;
;;   1. Ensuring the code is present in the final ELF file in a `.text` section.
;;   2. There is a `.text` section header for this section.
;;   3. There is a program header to ensure the code is loaded into memory.
;;   4. The entrypoint of the ELF file is set to wherever the above program
;;      header will place the code in memory.
;;
;; Note all of these will be finalized immediately, as references between
;; different parts of the file need to be finalized at the time of writing the
;; ELF file.
;;
;; @param elf - the in-progress ELF file structure
;; @param id - a unique ID used for this block of code. This ID won't be
;;             preserved in the output ELF file, but it is used internally
;;             during compilation to correlated related sections together
;; @param code-bytes - the list of bytes comprising the code
;; @return the updated ELF file structure
(define (add-executable-code elf id code-bytes)
  (let ((code-length (length code-bytes)))
    (add-stubs
      elf
      (list
        (new-stub-text id code-bytes)
        (new-stub-section-header-text id code-length)
        (new-stub-program-header-loadable id code-length))) ))

;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ELF-HEADER-SIZE 0x40)
(define SECTION-HEADER-SIZE 0x40)
(define PROGRAM-HEADER-SIZE 0x38)

(define SECTION-TYPE-PROGBITS 0x01)
(define SECTION-TYPE-STRTAB 0x03)

(define SEGMENT-TYPE-LOADABLE 0x01)
(define ADDRESS-ALIGNMENT 0x1000)
(define EXECUTABLE-MEMORY-BASE-ADDRESS 0x400000)

;; INTERNAL DATA STRUCTURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define elf->stubs (compose car cdr))

(define stub->type car)
(define stub->id (compose car cdr))
(define text-stub->code-bytes (compose car cdr cdr))
(define shstrtab-stub->shstrtab (compose car cdr cdr))
(define section-header-stub->type (compose car cdr cdr))
(define header-stub->associated-section-id (compose car cdr cdr cdr))

(define (new-stub-text id code-bytes)
  (list 'stub-text id code-bytes))

(define (new-stub-section-header-text associated-section-id size-in-bytes)
  ; TODO: remove magic numbers in favor of constants
  (list
    'stub-section-header
    'no-id
    'text
    associated-section-id
    SECTION-TYPE-PROGBITS
    0x06 ; Allocatable + executable
    'placeholder-address
    'placeholder-offset
    size-in-bytes ; Section size
    0x00 ; Link, none because of static binary
    0x00 ; No extra information
    ADDRESS-ALIGNMENT
    0x00)) ; Text section is not a table, no entry size

(define (new-stub-section-header-shstrtab shstrtab)
  (list
    'stub-section-header
    'no-id
    'shstrtab
    'shstrtab
    SECTION-TYPE-STRTAB
    0x00 ; No flags
    0x00 ; Address doesn't apply to string tables
    'placeholder-offset
    (shstrtab 'size-in-bytes '()) ; Section size
    0x00 ; Link, none because of static binary
    0x00 ; No extra information
    0x00 ; No address alignment
    0x00)) ; Text section is not a table, no entry size

(define (new-stub-program-header-loadable associated-section-id size-in-bytes)
  ; TODO: remove magic numbers in favor of constants
  (list
    'stub-program-header
    'no-id
    associated-section-id
    SEGMENT-TYPE-LOADABLE
    0x05 ; flags: readable + executable
    size-in-bytes
    ADDRESS-ALIGNMENT))

(define (new-stub-shstrtab shstrtab)
  (list 'stub-shstrtab 'shstrtab shstrtab))

(define (laid-out-stub type id data offset size)
  (list
    type
    id
    data
    offset
    size
    (+ offset size)))

(define laid-out-stub->type car)
(define laid-out-stub->id (compose car cdr))
(define laid-out-stub->data (compose car cdr cdr))
(define laid-out-stub->offset (compose car cdr cdr cdr))
(define laid-out-stub->size (compose car cdr cdr cdr cdr))
(define laid-out-stub->offset-after-section (compose car cdr cdr cdr cdr cdr))

(define (find-laid-out-stub-of-type laid-out-stubs type)
  (cond
    ((null? laid-out-stubs) (error-and-exit "No stub found of type: " type))
    ((= (laid-out-stub->type (car laid-out-stubs)) type) (car laid-out-stubs))
    (else (find-laid-out-stub-of-type (cdr laid-out-stubs) type)) ))

(define (laid-out-stub-with-id laid-out-stubs id)
  (cond
    ((null? laid-out-stubs) (error-and-exit "Stub with ID not found: " id))
    ((= (stub->id (car laid-out-stubs)) id) (car laid-out-stubs))
    (else (laid-out-stub-with-id (cdr laid-out-stubs) id)) ))

;; STUBS -> ELF DATA STRUCTURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (file-offset->executable-memory-address file-offset)
  (+ EXECUTABLE-MEMORY-BASE-ADDRESS file-offset))

(define (file-offset->executable-memory-address-if-placeholder
          given-memory-address
          file-offset)
  (if (number? given-memory-address)
    given-memory-address
    (file-offset->executable-memory-address file-offset) ))

;; OUTPUT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO - document
(define (emit-as-bytes elf)
  (let* ((shstrtab (construct-shstrtab elf))
         (elf
           (add-stubs
             elf
             (list
               (new-stub-section-header-shstrtab shstrtab)
               (new-stub-shstrtab shstrtab))))
         (laid-out-stubs (lay-out-stubs elf))
         (section-header-table
           (construct-section-header-table laid-out-stubs shstrtab))
         (program-header-table (construct-program-header-table laid-out-stubs))
         (remaining-sections (construct-non-header-sections elf))
         (header (construct-header laid-out-stubs)))
    ; Useful for debugging
    ; (display elf) (newline)
    ; (display laid-out-stubs) (newline)
    (append
      header
      section-header-table
      program-header-table
      remaining-sections)) )

(define (construct-header laid-out-stubs)
  (define (index-of-section-header section-headers type)
    (define (helper remaining-section-headers index-so-far)
      (cond
        ((null? remaining-section-headers)
         (error-and-exit "No section header stub found of type: " type))
        ((= (section-header-stub->type (car remaining-section-headers)) type)
         index-so-far)
        (else (helper (cdr remaining-section-headers) (+ index-so-far 1))) ))

    (helper section-headers 0))

  (let ((section-headers-stub
          (find-laid-out-stub-of-type laid-out-stubs 'stub-section-headers))
        (program-headers-stub
          (find-laid-out-stub-of-type laid-out-stubs 'stub-program-headers))

        (entrypoint
          (file-offset->executable-memory-address
            (laid-out-stub->offset
              (laid-out-stub-with-id laid-out-stubs 'main)))) )
    (append
      (list
        0x7f 0x45 0x4c 0x46 ; Magic number
        0x02 ; 64-bit architecture
        0x01 ; Two's compliment, little-endian
        0x01 ; Current ELF specification version
        0x00 0x00 ; "None" OS/ABI, equiv. to UNIX - System-V, default version
        0x00 0x00 0x00 0x00 0x00 0x00 0x00 ; Padding until 16 bytes

        0x02 0x00 ; Static executable
        0x3e 0x00 ; AMD x86-64
        0x01 0x00 0x00 0x00) ; Current file version

      (int->little-endian entrypoint 8) ; Entrypoint

      ; Program header table start
      (int->little-endian (laid-out-stub->offset program-headers-stub) 8)

      ; Section header table start
      (int->little-endian (laid-out-stub->offset section-headers-stub) 8)

      (list 0x00 0x00 0x00 0x00) ; Processsor-specific flags, none defined
      (int->little-endian ELF-HEADER-SIZE 2) ; ELF header size

      (list 0x38 0x00) ; Program header entry size, 56 bytes for 64-bit
                       ; architectures
      (int->little-endian (length (laid-out-stub->data program-headers-stub)) 2)

      (list 0x40 0x00) ; Section header entry size, 64 bytes for 64-bit
                       ; architectures
      (int->little-endian (length (laid-out-stub->data section-headers-stub)) 2)

      ; Section name string table index
      (int->little-endian
        (index-of-section-header
          (laid-out-stub->data section-headers-stub)
          'shstrtab)
        2) )))

(define (construct-shstrtab elf)
  ; TODO - depending on how the stubs are represented in the end, this approach
  ;   may need refinement. The assumption here is there is a one-to-one
  ;   correspondance between stubs and sections, not that multiple stubs may
  ;   make up a single section.
  (define (stub->header-name stub)
    (let ((type (stub->type stub)))
      (cond
        ((= type 'stub-text) (cons 'text ".text"))
        (else '())) ))

  (define (stubs->header-names stubs)
    (filter
      (lambda (name) (not (null? name)))
      (map stub->header-name stubs)))

  (define (ascii-string-to-bytes str)
    (if (str:null? str)
      '(0x00)
      (cons
        (ascii-char-to-byte (str:at str 0))
        (ascii-string-to-bytes (str:string-tail str 1))) ))

  (define (ascii-string-list-to-bytes strings)
    (if (null? strings)
      '()
      (append
        (ascii-string-to-bytes (car strings))
        (ascii-string-list-to-bytes (cdr strings))) ))

  (define (lookup-table-instance name-and-bytes-pairs)
    (define (offset-from-lookup-table pairs offset-so-far name)
      (cond
        ((null? pairs)
         (error-and-exit
           "Could not find section header name in table: "
           name))

        ((= (car (car pairs)) name) offset-so-far)

        (else
          (offset-from-lookup-table
            (cdr pairs)
            (+ offset-so-far (length (cdr (car pairs))))
            name)) ))

    (define (bytes-from-lookup-table pairs)
      (if (null? pairs)
        '()
        (append (cdr (car pairs)) (bytes-from-lookup-table (cdr pairs))) ))

    (define (self method args)
      (cond
        ((= method 'offset-for-name)
         (offset-from-lookup-table
           name-and-bytes-pairs
           0
           (car args)))

        ((= method 'bytes) (bytes-from-lookup-table name-and-bytes-pairs))

        ((= method 'size-in-bytes) (length (self 'bytes '())))

        (else
          (error-and-exit
            "Unknown method for shstrtab lookup table: "
            method)) ))

    self)

  (let*
    ((header-names (stubs->header-names (elf->stubs elf)))
     (header-names-with-added-strings
       (append
         (list (cons 'empty "")) ; the table must start with an empty string
         header-names
         (list (cons 'shstrtab ".shstrtab")))) ; shstrtab is itself a section!
     (lookup-table
       (map
         (lambda (entry-with-string)
           (cons
             (car entry-with-string)
             (ascii-string-to-bytes (cdr entry-with-string))))
         header-names-with-added-strings) ))
    (lookup-table-instance lookup-table) ))

(define (lay-out-stubs elf)
  (define (gather-section-headers stubs)
    (let ((headers
            (filter
              (lambda (stub) (= (stub->type stub) 'stub-section-header))
              stubs)))
    (laid-out-stub
      'stub-section-headers
      'no-id
      headers
      ELF-HEADER-SIZE
      (* SECTION-HEADER-SIZE (length headers))) ))

  (define (gather-program-headers stubs last-stub)
    (let ((headers
            (filter
              (lambda (stub) (= (stub->type stub) 'stub-program-header))
              stubs)))
      (laid-out-stub
        'stub-program-headers
        'no-id
        headers
        (laid-out-stub->offset-after-section last-stub)
        (* PROGRAM-HEADER-SIZE (length headers))) ))

  (define (gather-non-header-sections stubs last-stub)
    (define (size-of-stub stub)
      (let ((stub-type (stub->type stub)))
        (cond
          ((= stub-type 'stub-text) (length (text-stub->code-bytes stub)))
          ((= stub-type 'stub-shstrtab)
           ((shstrtab-stub->shstrtab stub) 'size-in-bytes))
          (else
            (error-and-exit
              "size-of-stub: unknown stub type"
              stub-type)) )))

    (define (helper remaining-stubs last-stub-so-far)
      (cond
        ((null? remaining-stubs) '())
        ((or (= (stub->type (car remaining-stubs)) 'stub-section-header)
             (= (stub->type (car remaining-stubs)) 'stub-program-header))
         (helper (cdr remaining-stubs) last-stub-so-far))
        (else
          (let* ((latest-stub (car remaining-stubs))
                 (latest-laid-out-stub
                   (laid-out-stub
                     (stub->type latest-stub)
                     (stub->id latest-stub)
                     latest-stub
                     (laid-out-stub->offset-after-section last-stub-so-far)
                     (size-of-stub latest-stub))))
            (cons
              latest-laid-out-stub
              (helper (cdr remaining-stubs) latest-laid-out-stub)))) ))

    (helper stubs last-stub))

  (let* ((stubs (elf->stubs elf))
         (section-headers (gather-section-headers stubs))
         (program-headers (gather-program-headers stubs section-headers))
         (non-header-sections (gather-non-header-sections stubs program-headers)))
    (append
      (list
        section-headers
        program-headers)
        non-header-sections) ))

(define (construct-section-header-table laid-out-stubs shstrtab)
  (define (section-header-stub->bytes laid-out-stubs stub)
    (let* (((stub-type
              id
              section-type
              associated-section-id
              section-type-bits
              flags
              potential-memory-address
              offset
              size
              link
              info
              alignment
              entry-size)
            stub)

           (offset-in-shstrtab (shstrtab 'offset-for-name (list section-type)))

           (associated-section-offset
             (laid-out-stub->offset
               (laid-out-stub-with-id laid-out-stubs associated-section-id)))

           (resolved-memory-address
             (file-offset->executable-memory-address-if-placeholder
               potential-memory-address
               associated-section-offset)) )
      (append
        (int->little-endian offset-in-shstrtab 4)
        (int->little-endian section-type-bits 4)
        (int->little-endian flags 8)
        (int->little-endian resolved-memory-address 8)
        (int->little-endian associated-section-offset 8)
        (int->little-endian size 8)
        (int->little-endian link 4)
        (int->little-endian info 4)
        (int->little-endian alignment 8)
        (int->little-endian entry-size 8)) ))

  (define (stub->section-header-entry-bytes laid-out-stubs stub)
    (if (not (= (stub->type stub) 'stub-section-header))
      '()
      (section-header-stub->bytes laid-out-stubs stub)))

  (define (process-stubs laid-out-stubs section-header-stubs)
    (if (null? section-header-stubs)
      '()
      (append
        (stub->section-header-entry-bytes
          laid-out-stubs
          (car section-header-stubs))
        (process-stubs laid-out-stubs (cdr section-header-stubs))) ) )

  (let* ((laid-out-section-headers-stub
           (find-laid-out-stub-of-type laid-out-stubs 'stub-section-headers))
         (section-header-stubs
           (laid-out-stub->data laid-out-section-headers-stub)))
    (process-stubs laid-out-stubs section-header-stubs) ))

(define (construct-program-header-table laid-out-stubs)
  (define (program-header-stub->bytes laid-out-stubs stub)
    (let* (((stub-type
              id
              associated-section-id
              segment-type-bits
              flags
              size
              alignment)
            stub)

           (associated-section-offset
             (laid-out-stub->offset
               (laid-out-stub-with-id laid-out-stubs associated-section-id)))

           (memory-address
             (file-offset->executable-memory-address associated-section-offset)))
      (append
        (int->little-endian segment-type-bits 4)
        (int->little-endian flags 4)
        (int->little-endian associated-section-offset 8) ; file offset
        (int->little-endian memory-address 8) ; virtual memory address
        (int->little-endian memory-address 8) ; physical memory address
        (int->little-endian size 8) ; segment size in file
        (int->little-endian size 8) ; segment size in memory (no compression)
        (int->little-endian alignment 8)) ))

  (define (stub->program-header-entry-bytes laid-out-stubs stub)
    (if (not (= (stub->type stub) 'stub-program-header))
      '()
      (program-header-stub->bytes laid-out-stubs stub)))

  (define (process-stubs laid-out-stubs program-header-stubs)
    (if (null? program-header-stubs)
      '()
      (append
        (stub->program-header-entry-bytes
          laid-out-stubs
          (car program-header-stubs))
        (process-stubs laid-out-stubs (cdr program-header-stubs))) ) )

  (let* ((laid-out-program-headers-stub
           (find-laid-out-stub-of-type laid-out-stubs 'stub-program-headers))
         (program-header-stubs
           (laid-out-stub->data laid-out-program-headers-stub)))
    (process-stubs laid-out-stubs program-header-stubs) ))

(define (construct-non-header-sections elf)
  (define (stub->bytes stub)
    (let ((stub-type (stub->type stub)))
      (cond
        ((= stub-type 'stub-text) (text-stub->code-bytes stub))
        ((= stub-type 'stub-shstrtab) ((shstrtab-stub->shstrtab stub) 'bytes))
        (else '()) )))

  (define (process-stubs stubs)
    (if (null? stubs)
      '()
      (append
        (stub->bytes (car stubs))
        (process-stubs (cdr stubs))) ) )

  (process-stubs (elf->stubs elf)) )

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  ; Constructors
  empty-static-executable

  ; Builder functions
  add-executable-code

  ; Output
  emit-as-bytes)
