#lang sicp
(#%require (only racket module+ format) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; (add-1 zero)
;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; (lambda (f) (lambda (x) (f x)))

(define one (lambda (f) (lambda (x) (f x))))

;; (add-1 one)
;; (lambda (f) (lambda (x) (f ((one f) x))))
;; (lambda (f) (lambda (x) (f (f x))))

(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (plus m n)
  (lambda (f) (compose (m f) (n f))))

(define (sqr n) (two n))
(define (times m n) (compose m n))
(define (pow m n) (n m))

(define (to-int n) ((n inc) 0))
(define (to-church n)
  (define (iter n res)
    (if (= n 0)
        res
        (iter (dec n) (add-1 res))))
  (iter n zero))

;; TODO - extend the Church number calculations

(module+ main
  (begin-example "2.6")
  (displn "(to-int zero)")
  (displn (to-int zero))
  (displn "(to-int one)")
  (displn (to-int one))
  (displn "(to-int two)")
  (displn (to-int two))
  (displn "(to-int (plus two two))")
  (displn (to-int (plus two two)))
  (displn "(to-int (plus one (plus two two)))")
  (displn (to-int (plus one (plus two two))))
  (displn "(to-int (sqr (plus two two)))")
  (displn (to-int (sqr (plus two two))))
  (displn "(map (compose to-int to-church) (iota 4))")
  (displn (map (compose to-int to-church) (iota 4)))
  (displn "(to-int (times two (times two two)))")
  (displn (to-int (times two (times two two))))
  (displn "(to-int (times two (times two three)))")
  (displn (to-int (times two (times two three))))
  (displn "(to-int (sqr two))")
  (displn (to-int (sqr two)))
  (displn "(to-int (sqr three))")
  (displn (to-int (sqr three)))
  (displn "(to-int (pow two three))")
  (displn (to-int (pow two three)))
  (displn "(to-int (pow three two))")
  (displn (to-int (pow three two)))
  )
