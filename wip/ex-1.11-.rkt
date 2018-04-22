#lang sicp

;; ex-1.11

(define (f3sum f1 f2 f3)
  (+ (* 1 f1)
     (* 2 f2)
     (* 3 f3))
  )

(define (fib3-rec n)
  (if (< n 3)
      n
      (f3sum
       (fib3-rec (- n 1))
       (fib3-rec (- n 2))
       (fib3-rec (- n 3)))))


(define (fib3-iter n)
  (define (iter i f1 f2 f3)
    (if (= i n)
        (f3sum f1 f2 f3)
        (iter (+ i 1)
              (f3sum f1 f2 f3)
              f1
              f2)))
  (if (< n 3)
      n
      (iter 3
            (fib3-iter 2)
            (fib3-iter 1)
            (fib3-iter 0))))

(map fib3-rec (list 0 1 2 3 4 5 6 7))

(map fib3-iter (list 0 1 2 3 4 5 6 7))

;; ex-1.12

(define (pas row col)
  "row and col are indexed from 0" 
  (cond
    ((< row 0) (error "negative row" row))
    ((< col 0) (error "negative col" col))
    ((> col row) (error " col > row" col row))
    ((= col row) 1)
    ((= col 0) 1)
    (else (+ (pas (- row 1) col)
             (pas (- row 1) (- col 1))))))

(define (from-to a b)
  (define (iter curlist curval)
    (if (>= curval a)
        (iter (cons curval curlist) (dec curval))
        curlist))
  (iter nil (dec b)))

(define (pas-row n)
  (map
   (lambda (col)
     (pas n col))
   (from-to 0 (inc n))))

(define (pas-tri n)
  (map pas-row
       (from-to 0 (inc n)))
  )

(pas-tri 6)

;; ex-1.13 - obvious

