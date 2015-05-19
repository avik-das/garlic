(require http)

(define *port* 8000)

(define (handle-request)
  (display "handling request")
  (newline)
  "<html><head><title>garlic HTTP</title></head><body><p>This is being served by garlic code!</p><p>This is amazing!</p></body></html>")

(http:serve *port* handle-request)
