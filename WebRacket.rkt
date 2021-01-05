#lang racket

(require xml net/url
         html
         racket/control
         racket/dict)

(provide listen-on
         app-get
         app-get-keys
         app-post
         app-static
         send)

;-----heavy systems programming web server stuff-----
; adapted from racket systems programming tutorial:
; https://docs.racket-lang.org/more/------
; Basic Web Server
(define (listen-on port-number)
  (define main-cust (make-custodian))
  (parameterize ([current-custodian main-cust])
    (define listener (tcp-listen port-number 5 #t))
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
      ;; 404 page
      `(html (head (title "Error: 404"))
             (body
              (font ((color ""))
                    "No resource found at "
                    ,str-path)))))

(define dispatch-table (make-hash))

(hash-set! dispatch-table "hello"
           (lambda (query)
             `(html (body "Hello, World!"))))

(hash-set! dispatch-table "app"
           (lambda (query)
             "5"))

(define (build-request-page path html)
  (display "I am a function"))

;---------WebRacket Functions-------------;

(define (app-get path sender)
  (hash-set! dispatch-table path sender))

(define (app-post path)
  (display "I am a post request place thing"))

(define (app-static file-path)
  (display "I give you static"))

(define (app-get-variable path variable)
  (hash-set! dispatch-table path ))

(define (send-file file-path)
  (display "I send files!"))

(define (app-get-keys dict read?)
  (map (λ (key) (app-get key (send (read? key)))) (dict-keys dict)))

(define (send x)
      (lambda (query)
        x))

;------Example application (leave it commented out! aaaaaaah)----;

; (app-get "main" (send "♫ I want to be the very best, like no one ever was ♫"))

; (define list-of-things '("apple" "bannana" "creation" "God" "point?"))

; (app-get-list list-of-things (send "

; (listen-on 3000)

; (hash-set! dispatch-table "hello/e"
           (λ (query)
                   "e"))
; (app-get "" (send "o"))
; (app-get "/" (send "333030"))

; (app-get "ee" (send `(html (body (h1 "Hello!")))))



; (listen-on 3000)
