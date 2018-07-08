#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(module+ main
  (begin-example "2.53")
  (list 'a 'b 'c)
  ;; expect (a b c)
  ;; actual (a b c)
  
  (list (list 'george))
  ;; expect ((george))
  ;; actual ((george))
  
  (cdr '((x1 x2) (y1 y2)))
  ;; expect ((y1 y2))
  ;; actual ((y1 y2))
  
  (cadr '((x1 x2) (y1 y2)))
  ;; expect (y1 y2)
  ;; actual (y1 y2)

  (pair? (car '(a short list)))
  ;; expect #f
  ;; actual #f

  (memq 'red '((red shoes) (blue socks)))
  ;; expect #f
  ;; actual #f
  
  (memq 'red '(red shoes blue socks))
  ;; expect (red shoes blue socks)
  ;; actual (red shoes blue socks)
  
  )

(define (my-equal? a b)
  (cond
   ((and (symbol? a) (symbol? b))
    (eq? a b))
   ((and (null? a) (null? b))
    #t)
   ((and (list? a) (not (null? a)) (list? b) (not (null? b)))
    (and (eq? (car a) (car b))
         (my-equal? (cdr a) (cdr b))))
   (else #f)))
    
(module+ main
  (begin-example "2.54")
  (displn-eval '(my-equal? '(this is a list) '(this is a list)) ns)
  (displn-eval '(my-equal? '(this is a list) '(this (is a) list)) ns))

;; 2.55
;; (car ''abracadabra)
;; (car (quote (quote abracadabra)))
;; (car (list 'quote 'abracadabra))
;; => rep of 'quote, i.e. "quote"
