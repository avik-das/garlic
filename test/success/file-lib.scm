(require file)

; This test is run from the root of the garlic repository, so all file paths
; are relative to that directory.
(display (file:read-text "test/success/file-lib.scm"))
