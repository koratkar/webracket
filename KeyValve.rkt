#lang racket

(require "WebRacket.rkt")
(require racket/dict)

;; Database functions
(define mem-list (make-hash))

(define (create key value)
  (if (dict-has-key? mem-list key)
        #f
	(dict-set! mem-list key value)))

(define (read key)
  (if (dict-has-key? mem-list key)
	(dict-ref mem-list key)
        #f))

(define (update key new)
  (if (read key)
	(dict-update! mem-list key (lambda (n) new))
        (create key new)))

(define (update-key-name key new-name)
  (if (read key)
      (begin
        (create new-name (read key))
        (delete key))
      #f))
             
(define (delete key)
  (if (read key)
	(dict-remove! mem-list key)
        #f))

(define how-many?
  (hash-count mem-list))

;;;;; web server 

(create "Meaning" "43")


(listen-on 3000)
