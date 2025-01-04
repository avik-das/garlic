(require assoc)

(define base-alist
  (let* ((al0 (assoc:empty))
         (al1 (assoc:add al0 'a 100))
         (al2 (assoc:add al1 'b 200))
         (al3 (assoc:add al2 'a 101)) ; shadows original entry
         (al4 (assoc:add al3 'c 300)))
    al4))

(display (assoc:get base-alist 'a)) (newline)
(display (assoc:get base-alist 'b)) (newline)
(display (assoc:get base-alist 'c)) (newline)
(display (assoc:get base-alist 'd)) (newline)
(display (assoc:get (assoc:add base-alist 'd 400) 'd)) (newline)
(display (assoc:get (assoc:add base-alist 'b 201) 'd)) (newline)
(display (assoc:get (assoc:add base-alist 'b 201) 'b)) (newline)

(newline)

(display (assoc:has-key? base-alist 'a)) (newline)
(display (assoc:has-key? base-alist 'b)) (newline)
(display (assoc:has-key? base-alist 'c)) (newline)
(display (assoc:has-key? base-alist 'd)) (newline)
(display (assoc:has-key? (assoc:add base-alist 'd 400) 'd)) (newline)
(display (assoc:has-key? (assoc:add base-alist 'b 201) 'd)) (newline)
(display (assoc:has-key? (assoc:add base-alist 'b 201) 'b)) (newline)
