#lang sicp
(#%require (only racket module+ format) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")


(define (make-rat n d)
  (let ((norm (* (gcd n d) (sgn d))))
    (cons (/ n norm) (/ d norm))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(module+ main
  (begin-example "2.1")
  (define (res rat) (list (numer rat) (denom rat)))
  (let ((ns (iota 9 4 -1))
        (ds (append (iota 4 -4) (iota 4 1))))
     (for-each
     (lambda (n)
       (displn (map
        (lambda (d)
          (res (make-rat n d)))
        ds)))
     ns)))

(define (make-segment start end) (cons start end))
(define (start-segment segment) (car segment))
(define (end-segment segment) (cdr segment))

(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (midpoint-segment segment)
  (make-point
   (average (x-point (start-segment segment)) (x-point (end-segment segment)))
   (average (y-point (start-segment segment)) (y-point (end-segment segment)))))

(define (print-point point)
  (display "(")
  (display (x-point point))
  (display ",")
  (display (y-point point))
  (display ")"))

(module+ main
  (begin-example "2.2")
  (displn "mid point of (1,2) (3,4)")
  (print-point
   (midpoint-segment
    (make-segment
     (make-point 1 2)
     (make-point 3 4))))
  (newline))

(define (disp-segment segment)
  (make-point
   (- (x-point (end-segment segment)) (x-point (start-segment segment)))
   (- (y-point (end-segment segment)) (y-point (start-segment segment)))))
(define (norm-point p)
  (sqrt (+ (square (x-point p)) (square (y-point p)))))
(define (length-segment segment)
  (norm-point (disp-segment segment)))

(define (make-rect segment height)
  (list segment height))
(define (segment-rect rect) (car rect))
(define (width-rect rect) (length-segment (segment-rect rect)))
(define (height-rect rect) (cadr rect))

;; alternative rep
(define (make-rec width height angle shift)
  (list width height angle shift))

(define (width-rec rec) (car rec))
(define (height-rec rec) (cadr rec))
;; (define (angle-rec rec) (caddr rec))
;; (define (shift-rec rec) (cadddr rec))

(define (area-rect rect)
  (* (width-rect rect) (height-rect rect)))
(define (perim-rect rect)
  (* 2 (+ (width-rect rect) (height-rect rect))))

(module+ main
  (begin-example "2.3")
  (displn "area / perimeter of rect with sides 5 and 2")
  (let ((rect (make-rect (make-segment (make-point 1 1) (make-point 4 5))
                         2)))
    (displn (list (area-rect rect) (perim-rect rect)))))


(define (-cons x y) 
  (lambda (m) (m x y)))

(define (-car z) 
  (z (lambda (p q) p)))


;; expand (-car (-cons x y))
;; (-car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; x

(module+ main
  (begin-example "2.4")
  (displn "(-car (-cons 2 4))")
  (displn (-car (-cons 2 4))))


;; a,b <-> 2^a * 3^b

(define (/cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (power-of a n)
  (define (iter res n)
    (if (= (remainder n a) 0)
        (iter (inc res) (/ n a))
        res))
  (iter 0 n))

(define (/car p) (power-of 2 p))
(define (/cdr p) (power-of 3 p))

(module+ main
  (begin-example "2.5")
  (displn "(/car (/cons 2 4))")
  (displn (/car (/cons 2 4)))
  (displn "(/cdr (/cons 2 4))")
  (displn (/cdr (/cons 2 4))))

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

(define (plus m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

(define sqr (lambda (f) (lambda (x) (f (f x)))))
(define times (lambda (f g) (lambda (x) (f (g x)))))

(define (to-int n) ((n inc) 0))
(define (to-church n)
  (define (iter n res)
    (if (= n 0)
        res
        (iter (dec n) (add-1 res))))
  (iter n zero))

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
  )





