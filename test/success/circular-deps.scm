(require "../aux/circ1")
(require "../aux/circ2")

(newline)

(display (circ1:no-deps)) (newline)
(display (circ2:no-deps)) (newline)

(display (circ1:depends-on-circ2)) (newline)
(display (circ2:depends-on-circ1)) (newline)
