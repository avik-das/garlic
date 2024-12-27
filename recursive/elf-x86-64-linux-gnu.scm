;; An in-memory representation and associated functions for the ELF file format.
;; The scope of this module is reduced by assuming system configuration such as
;; x86-64 little-endian systems.

(require string => str)

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
;; @param code-bytes - the list of bytes comprising the code
;; @return the updated ELF file structure
(define (add-executable-code elf code-bytes)
  (add-stubs
    elf
    (list
      (new-stub-text code-bytes)
      (new-stub-section-header-text (length code-bytes))
      ; TODO - stub for program header
      )) )

;; CONSTANTS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define SECTION_TYPE_PROGBITS 0x01)
(define SECTION_TYPE_STRTAB 0x03)
(define ADDRESS_ALIGNMENT 0x1000)

;; INTERNAL DATA STRUCTURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define elf->stubs (compose car cdr))

(define stub->type car)

(define (new-stub-text code-bytes)
  (list 'stub-text code-bytes))

(define (new-stub-section-header-text size-in-bytes)
  ; TODO: remove magic numbers in favor of constants
  (list
    'stub-section-header
    'text
    SECTION_TYPE_PROGBITS
    0x06 ; Allocatable + executable
    'placeholder-address
    'placeholder-offset
    size-in-bytes ; Section size
    0x00 ; Link, none because of static binary
    0x00 ; No extra information
    ADDRESS_ALIGNMENT
    0x00)) ; Text section is not a table, no entry size

(define (new-stub-section-header-shstrtab shstrtab)
  ; TODO: remove magic numbers in favor of constants
  (list
    'stub-section-header
    'shstrtab
    SECTION_TYPE_STRTAB
    0x00 ; No flags
    0x00 ; Address and offset doesn't apply to string tables
    'placeholder-offset
    (shstrtab 'size-in-bytes '()) ; Section size
    0x00 ; Link, none because of static binary
    0x00 ; No extra information
    0x00 ; No address alignment
    0x00)) ; Text section is not a table, no entry size

; (define elf->sections (compose cdr car))
; (define elf->program-headers (compose cdr cdr car))
; 
; (define (located-byte-span byte-span offset)
;   (cons byte-span offset))
; (define located-byte-span->bytes car)
; (define located-byte-span->offset cdr)
; (define located-byte-span->length (compose located-byte-span->bytes length))
; (define (located-byte-span->offset-after span)
;   (+
;     (located-byte-span->offset span)
;     (located-byte-span->length span)))
; 
; (define section-type car)
; 
; (define (text-section code-bytes)
;   (cons 'text code-bytes))
; (define text-section->code-bytes cdr)

;; STUBS -> ELF DATA STRUCTURES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (int->little-endian int pad-to-total-bytes)
  ; 0x12345678 -> 0x78 0x56 0x34 0x12
  ; 0x00005678 -> 0x78 0x56 0x00 0x00
  (define (convert-remaining-bytes reduced-int num-bytes-left)
    (if (= num-bytes-left 0)
      '()
      (cons
        (bitwise-and reduced-int 0xff)
        (convert-remaining-bytes
          (arithmetic-shift reduced-int -8)
          (- num-bytes-left 1)) ) ))

  (convert-remaining-bytes int pad-to-total-bytes))

(define (num-section-headers elf)
  (define (num-section-headers-for-stub stub)
    (let ((type (stub->type stub)))
      (cond ((= type 'stub-text) 1) ; Later, may contribute more sections
            (else 0)) ))

  ((compose
     (lambda (s) (+ s 1)) ; Always include an entry for the section header
                          ;   string table, which is needed because there is a
                          ;   section header table in the first place
     sum
     (lambda (stubs) (map num-section-headers-for-stub stubs))
     elf->stubs) elf))

(define (num-program-headers elf)
  (define (num-program-headers-for-stub stub)
    (let ((type (stub->type stub)))
      (cond ((= type 'stub-text) 1)
            (else 0)) ))

  ((compose
     sum
     (lambda (stubs) (map num-program-headers-for-stub stubs))
     elf->stubs) elf))

;; OUTPUT ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO - document
(define (emit-as-bytes elf)
  (let* ((shstrtab (construct-shstrtab elf))
         (elf (add-stub elf (new-stub-section-header-shstrtab shstrtab)))
         (section-header-table (construct-section-header-table elf shstrtab))
         (header (construct-header elf)))
    ; Useful for debugging
    ; (display elf) (newline)
    (append
      header
      section-header-table
      (shstrtab 'bytes '()))) )

; TODO - there are two approaches to constructing the header:
;
; 1. Construct the header as a stub and fill in the details later, like all the
;    other sections.
; 2. Construct the header last, and pass in information from all the other
;    sections that have already been generated by this point. Currently, this is
;    the approach taken here, though passing in other information is still TBD.
(define (construct-header elf)
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

    ; TODO: 8 bytes -- entry point
    (list 0xff 0xff 0xff 0xff 0xff 0xff 0xff 0xff)

    ; TODO: 8 bytes -- program header table start
    (list 0xfe 0xfe 0xfe 0xfe 0xfe 0xfe 0xfe 0xfe)

    ; TODO: 8 bytes -- section header table start
    (list 0xfd 0xfd 0xfd 0xfd 0xfd 0xfd 0xfd 0xfd)

    (list
      0x00 0x00 0x00 0x00 ;  Processsor-specific flags, none defined
      0x40 0x00) ; ELF header size

    (list 0x38 0x00) ; Program header entry size, 56 bytes for 64-bit
                     ; architectures
    (int->little-endian (num-program-headers elf) 2)

    (list 0x40 0x00) ; Section header entry size, 64 bytes for 64-bit
                     ; architectures
    (int->little-endian (num-section-headers elf) 2)

    ; TODO: 2 bytes -- section name string table index
    (list 0xfd 0xfd) ))

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

(define (construct-section-header-table elf shstrtab)
  (define (section-header-stub->bytes stub)
    (let* (((stub-type
              section-type
              section-type-bits
              flags
              address
              offset
              size
              link
              info
              alignment
              entry-size)
            stub)

           (offset-in-shstrtab (shstrtab 'offset-for-name (list section-type))))
      (append
        (int->little-endian offset-in-shstrtab 4)
        (int->little-endian section-type-bits 4)
        (int->little-endian flags 8)
        '(0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00) ; TODO: address - may need resolution
        '(0x00 0x00 0x00 0x00 0x00 0x00 0x00 0x00) ; TODO: offset - may need resolution
        (int->little-endian size 8)
        (int->little-endian link 4)
        (int->little-endian info 4)
        (int->little-endian alignment 8)
        (int->little-endian entry-size 8)) ))

  (define (stub->section-header-entry-bytes stub)
    (if (not (= (stub->type stub) 'stub-section-header))
      '()
      (section-header-stub->bytes stub)))

  (define (process-stubs stubs)
    (if (null? stubs)
      '()
      (append
        (stub->section-header-entry-bytes (car stubs))
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

;; TEMP: MAIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; To be removed once the functionality has been tested, and the code generator
;; starts using this functionality.

(require "compiler_temp")

(define test-code
  '(0x48 0xc7 0xc0 0x3c 0x00 0x00 0x00 ; mov  $60, %rax
    0xbf 0x2a 0x00 0x00 0x00           ; mov  $42, %edi
    0x0f 0x05))                        ; syscall

((compose
   compiler_temp:print-bytes
   (lambda (e) (emit-as-bytes e))
   (lambda (e) (add-executable-code e test-code)))
 (empty-static-executable))
