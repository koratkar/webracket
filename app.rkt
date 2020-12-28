#lang racket

;; See "<<<" for two small changes, then jump down
;; to `send/suspend'.

(require xml net/url
         racket/control) ;; <<< new import

(define (serve port-no)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-no 5 #t))
    (define (loop)
      (accept-and-handle listener)
      (loop))
    (thread loop))
  (lambda ()
    (custodian-shutdown-all main-cust)))

(define (accept-and-handle listener)
  (define cust (make-custodian))
  (custodian-limit-memory cust (* 50 1024 1024))
  (parameterize ([current-custodian cust])
    (define-values (in out) (tcp-accept listener))
    (thread (lambda ()
              (handle in out)
              (close-input-port in)
              (close-output-port out))))
  ;; Watcher thread:
  (thread (lambda ()
            (sleep 10)
            (custodian-shutdown-all cust))))

(define (handle in out)
  (define req
    ;; Match the first line to extract the request:
    (regexp-match #rx"^GET (.+) HTTP/[0-9]+\\.[0-9]+"
                  (read-line in)))
  (when req
    ;; Discard the rest of the header (up to blank line):
    (regexp-match #rx"(\r\n|^)\r\n" in)
    ;; Dispatch:
    (let ([xexpr (prompt (dispatch (list-ref req 1)))]) ;; <<< changed
      ;; Send reply:
      (display "HTTP/1.0 200 Okay\r\n" out)
      (display "Server: k\r\nContent-Type: text/html\r\n\r\n" out)
      (display (xexpr->string xexpr) out))))

(define (dispatch str-path)
  ;; Parse the request as a URL:
  (define url (string->url str-path))
  ;; Extract the path part:
  (define path (map path/param-path (url-path url)))
  ;; Find a handler based on the path's first element:
  (define h (hash-ref dispatch-table (car path) #f))
  (if h
      ;; Call a handler:
      (h (url-query url))
      ;; No handler found:
      `(html (head (title "Error"))
             (body
              (font ((color "red"))
                    "Unknown page: "
                    ,str-path)))))

(define dispatch-table (make-hash))

(hash-set! dispatch-table "hello"
           (lambda (query)
             `(html (body "Hello, World!"))))

(define (build-request-page path html))

(define (many query)
  ;; Create a page containing the form:
  (build-request-page "Number of greetings:" "/reply" ""))

(define (reply query)
  ;; Extract and use the form results:
  (define n (string->number (cdr (assq 'number query))))
  `(html (body ,@(for/list ([i (in-range n)])
                   " hello"))))

;Old app-get
(define (get-send path html)
  (if (equal? path "/")
      (hash-set! dispatch-table ""
             (lambda (query)
               html))
      (hash-set! dispatch-table path
                 (lambda (query)
                   html))))


(hash-set! dispatch-table "many" many)
(hash-set! dispatch-table "reply" reply)

(define (listen-on number)
  (serve number))
;---------------------
; from here it should look like express, and I can put the above code in seprate file


           
             

(listen-on 3000)
