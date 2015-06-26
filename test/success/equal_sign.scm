(require "../aux/auxillary-module" => am)

;; Numbers
(display (= 1 1)) (newline)
(display (= 1 1 1 1)) (newline)
(display (= 1 1 2 1)) (newline)

(newline)

;; Symbols
(display (= 'a 'a)) (newline)
(display (= 'a 'a 'a 'a)) (newline)
(display (= 'a 'a 'b 'a)) (newline)

; symbols should be unique across modules
(display (= 'auxillary-symbol am:auxillary-symbol)) (newline)
