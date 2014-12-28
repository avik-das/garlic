; libctest is a C module, not a scheme one
(require libctest)
(display (libctest:return_scm_num)) (newline)
(display (+ (libctest:return_scm_num) 2)) (newline)
