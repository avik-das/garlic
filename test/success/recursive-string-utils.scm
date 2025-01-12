(require "../../recursive/string-utils" => su)

(display (su:ascii-string-to-bytes "abcd")) (newline)
(display (su:ascii-string-to-bytes "1234\t\nabcd\\\"")) (newline)
