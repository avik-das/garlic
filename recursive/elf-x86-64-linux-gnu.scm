;; An in-memory representation and associated functions for the ELF file format.
;; The scope of this module is reduced by assuming system configuration such as
;; x86-64 little-endian systems.

(require assoc)
(require file)
(require string => str)
(require "string-utils" *)
(require "byte-utils" *)

;; CONSTRUCTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (empty-static-executable)
  (add-stub
    (new-elf '())
    (new-stub-section-header-null)) )

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
;; Not all of these will be finalized immediately, as references between
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
  (define (compute-code-length)
    (define (byte-size byte)
      (if (data-ref? byte)
        (data-ref-get-size byte)
        1))

    (reduce
      (lambda (s byte) (+ s (byte-size byte)))
      0
      code-bytes) )

  (let ((code-length (compute-code-length code-bytes)))
    (add-stubs
      elf
      (list
        (new-stub-text id code-bytes code-length)
        (new-stub-section-header-text id code-length)
        (new-stub-program-header-text id code-length))) ))

;; Add the given data (represented as an opaque list of bytes) into the ELF
;; file as the data section that will be loaded into memory when the ELF file
;; is run. This entails:
;;
;;   1. Ensuring the data is present in the final ELF file in a `.data` section.
;;   2. There is a `.data` section header for this section.
;;   3. There is a program header to ensure the data is loaded into memory.
;;
;; Not all of these will be finalized immediately, as references between
;; different parts of the file need to be finalized at the time of writing the
;; ELF file.
;;
;; @param elf - the in-progress ELF file structure
;; @param bytes - the list of bytes comprising the data section
;; @return the updated ELF file structure
(define (add-data elf labels-and-bytes)
  (define (state-new size offsets) (list size offsets))
  (define (state-empty) (state-new 0 (assoc:empty)))

  (define state->offsets (compose car cdr))

  (define (state->next state label-and-bytes)
    (let (((prev-size prev-offsets) state)
          ((label . bytes) label-and-bytes))
      (state-new
        (+ prev-size (length bytes))
        (assoc:add prev-offsets label prev-size)) ))

  (define (compute-size-and-offsets labels-and-bytes-pairs)
    (reduce state->next (state-empty) labels-and-bytes-pairs) )

  (define (collate-bytes labels-and-bytes-pairs)
    (reduce
      (lambda (bytes label-and-bytes) (append bytes (cdr label-and-bytes)))
      '()
      labels-and-bytes-pairs) )

  (let* ((labels-and-bytes-pairs (assoc:pairs labels-and-bytes))
         ((size offsets) (compute-size-and-offsets labels-and-bytes-pairs))
         (all-bytes (collate-bytes labels-and-bytes-pairs)))
    (add-stubs
      elf
      (list
        (new-stub-data 'data all-bytes offsets)
        (new-stub-section-header-data 'data size)
        (new-stub-program-header-data 'data size))) ))

;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ELF-HEADER-SIZE 0x40)
(define SECTION-HEADER-SIZE 0x40)
(define PROGRAM-HEADER-SIZE 0x38)

(define SECTION-TYPE-PROGBITS 0x01)
(define SECTION-TYPE-STRTAB 0x03)

(define SEGMENT-TYPE-LOADABLE 0x01)
(define ADDRESS-ALIGNMENT 0x1000)
(define EXECUTABLE-MEMORY-BASE-ADDRESS 0x400000)
(define DATA-MEMORY-BASE-ADDRESS 0x800000)

;; EXTERNAL DATA STRUCTURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (data-ref label size-in-bytes)
  (list 'data-ref label size-in-bytes))

(define (data-ref? val)
  (and
    (list? val)
    (not (null? val))
    (= (car val) 'data-ref)) )
(define data-ref-get-label (compose car cdr))
(define data-ref-get-size (compose car cdr cdr))

;; INTERNAL DATA STRUCTURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define elf->stubs (compose car cdr))

(define stub->type car)
(define stub->id (compose car cdr))
(define text-stub->code-bytes (compose car cdr cdr))
(define text-stub->size-in-bytes (compose car cdr cdr cdr))
(define data-stub->bytes (compose car cdr cdr))
(define data-stub->offsets-by-label (compose car cdr cdr cdr))
(define shstrtab-stub->shstrtab (compose car cdr cdr))
(define section-header-stub->type (compose car cdr cdr))
(define header-stub->associated-section-id (compose car cdr cdr cdr))

(define (new-stub-text id code-bytes size-in-bytes)
  (list 'stub-text id code-bytes size-in-bytes))

(define (new-stub-data id bytes offsets-by-label)
  (list 'stub-data id bytes offsets-by-label))

(define (new-stub-section-header-null)
  (list
    'stub-section-header
    'no-id
    'null
    'null
    0x00
    0x00
    0x00
    0x00
    0x00
    0x00
    0x00
    0x00
    0x00))

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

(define (new-stub-section-header-data associated-section-id size-in-bytes)
  ; TODO: remove magic numbers in favor of constants
  (list
    'stub-section-header
    'no-id
    'data
    associated-section-id
    SECTION-TYPE-PROGBITS
    0x03 ; Allocatable + writeable
    'placeholder-address
    'placeholder-offset
    size-in-bytes ; Section size
    0x00 ; Link, none because of static binary
    0x00 ; No extra information
    ADDRESS-ALIGNMENT
    0x00)) ; Data section is not a table, no entry size

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

(define (new-stub-program-header-text associated-section-id size-in-bytes)
  ; TODO: remove magic numbers in favor of constants
  (list
    'stub-program-header
    'no-id
    'text
    associated-section-id
    SEGMENT-TYPE-LOADABLE
    0x05 ; flags: readable + executable
    size-in-bytes
    ADDRESS-ALIGNMENT))

(define (new-stub-program-header-data associated-section-id size-in-bytes)
  ; TODO: remove magic numbers in favor of constants
  (list
    'stub-program-header
    'no-id
    'data
    associated-section-id
    SEGMENT-TYPE-LOADABLE
    0x06 ; flags: readable + writeable
    size-in-bytes
    ADDRESS-ALIGNMENT))

(define (new-stub-shstrtab shstrtab)
  (list 'stub-shstrtab 'shstrtab shstrtab))

(define (text-stub->resolved-code-bytes stub laid-out-stubs)
  (define (address-for-label laid-out-data-stub label)
    (let* ((data-file-offset (laid-out-stub->offset laid-out-data-stub))
           (base-address (file-offset->memory-address 'data data-file-offset))
           (data-stub (laid-out-stub->data laid-out-data-stub))
           (label-offset (data-stub->offset-for-label data-stub label)))
      (+ base-address label-offset) ))

  (define (unresolved-byte-to-resolved-bytes laid-out-data-stub byte)
    (if (data-ref? byte)
      (byte-utils:int->little-endian
        (address-for-label laid-out-data-stub (data-ref-get-label byte))
        (data-ref-get-size byte))
      (list byte)) )

  (define (resolve-data-refs laid-out-data-stub unresolved-bytes)
    (reduce
      (lambda (bytes byte)
        (append
          bytes
          (unresolved-byte-to-resolved-bytes laid-out-data-stub byte)))
      '()
      (text-stub->code-bytes stub)) )


  (let ((unresolved-bytes (text-stub->code-bytes stub)))
    (if (any? (compose not number?) unresolved-bytes)
      (resolve-data-refs
        (find-laid-out-stub-of-type laid-out-stubs 'stub-data)
        unresolved-bytes)
      unresolved-bytes) ))

(define (data-stub->offset-for-label stub label)
  (let* ((offsets (data-stub->offsets-by-label stub))
         (label-and-offset
           (find (lambda (pair) (= (car pair) label)) offsets)))
    (if (not label-and-offset)
      (error-and-exit
        "data-stub->offset-for-label: "
        "Referenced label not found in data section: "
        label)
      (cdr label-and-offset)) ))

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

(define (file-offset->memory-address section-type file-offset)
  (+
    file-offset
    (cond
      ((= section-type 'text) EXECUTABLE-MEMORY-BASE-ADDRESS)
      ((= section-type 'data) DATA-MEMORY-BASE-ADDRESS)
      (else
        (error-and-exit
          "file-offset->memory-address: unknown section-type: "
          section-type))) ))

(define (file-offset->memory-address-if-placeholder
          section-type
          given-memory-address
          file-offset)
  (if (number? given-memory-address)
    given-memory-address
    (file-offset->memory-address section-type file-offset) ))

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
         (remaining-sections (construct-non-header-sections laid-out-stubs))
         (header (construct-header laid-out-stubs)))
    ; Useful for debugging
    ; (display elf) (newline)
    ; (display laid-out-stubs) (newline)
    (append-in-place
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
          (file-offset->memory-address
            'text
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
        ((= type 'stub-data) (cons 'data ".data"))
        ((= type 'stub-section-header) '())
        ((= type 'stub-program-header) '())
        (else
          (error-and-exit
            "stub->header-name: unknown stub type: "
            type))) ))

  (define (stubs->header-names stubs)
    (filter
      (lambda (name) (not (null? name)))
      (map stub->header-name stubs)))

  (define (lookup-table-instance name-and-bytes-pairs)
    (define (offset-from-lookup-table pairs offset-so-far name)
      (cond
        ; The null section header is special in that it has a constant. Note
        ; that we could have artificially inserted an entry for the null section
        ; header into the table, but that would introduce problems when
        ; converting the table into bytes: if the null section header is not
        ; present, we would still need to introduce a NULL byte at the begining
        ; of the serialized output.
        ;
        ; Instead, always introduce a NULL byte at the beginning of the
        ; serialized output, and hardcode the offset for the null section
        ; header's name as that first byte.
        ((= name 'null) 0)

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
          ((= stub-type 'stub-text) (text-stub->size-in-bytes stub))
          ((= stub-type 'stub-data) (length (data-stub->bytes stub)))
          ((= stub-type 'stub-shstrtab)
           ((shstrtab-stub->shstrtab stub) 'size-in-bytes))
          (else
            (error-and-exit
              "size-of-stub: unknown stub type: "
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
             ; The null section header is special in that it has no associated
             ; section. The memory address is not a placeholder in the section
             ; header stub anyway, so it's okay to just default this offset to
             ; zero.
             (if (= section-type 'null)
               0
               (laid-out-stub->offset
                 (laid-out-stub-with-id laid-out-stubs associated-section-id))))

           (resolved-memory-address
             (file-offset->memory-address-if-placeholder
               section-type
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
              associated-section-type
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
             (file-offset->memory-address
               associated-section-type
               associated-section-offset)))
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

(define (construct-non-header-sections laid-out-stubs)
  (define (laid-out-stub->bytes laid-out-stub)
    (let ((stub-type (laid-out-stub->type laid-out-stub))
          (stub (laid-out-stub->data laid-out-stub)))
      (cond
        ((= stub-type 'stub-text)
         (text-stub->resolved-code-bytes stub laid-out-stubs))

        ((= stub-type 'stub-data) (data-stub->bytes stub))
        ((= stub-type 'stub-shstrtab) ((shstrtab-stub->shstrtab stub) 'bytes))
        ((= stub-type 'stub-section-headers) '())
        ((= stub-type 'stub-program-headers) '())
        (else
          (error-and-exit
            "stub->bytes: unknown stub type: "
            stub-type)) )))

  (define (process-stubs stubs)
    (if (null? stubs)
      '()
      (append
        (laid-out-stub->bytes (car stubs))
        (process-stubs (cdr stubs))) ) )

  (process-stubs laid-out-stubs) )

;; EXPORTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module-export
  ; Constructors
  empty-static-executable

  ; Builder functions
  add-executable-code
  add-data

  ; Data structures
  data-ref

  ; Output
  emit-as-bytes)
