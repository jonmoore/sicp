#lang sicp


(#%provide λ)
;; treat λ and lambda as equivalent
(define-syntax λ
  (syntax-rules ()
    ((_ . more) (lambda . more))))

(#%provide make-timer)
(define (make-timer)
  (define (timer-from start)
    (lambda ()
      (/ (- (runtime) start) 1e6)))
  (timer-from (runtime)))

(#%provide filter)
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(#%provide fold-left)
(define (fold-left proc init list)
  (if (null? list)
      init
      (fold-left proc (proc init (car list)) (cdr list))))

(#%provide sum-list)
(define (sum-list list)
  (fold-left + 0 list))

(#%provide constant-fn)
(define (constant-fn x)
  (lambda () x))

(#%provide length)
(define (length list)
  (fold-left
   (λ (length-so-far _) (inc length-so-far))
   0 list))

(#%provide average)
(define (average list)
  (/ (sum-list list) (length list)))

(#%provide atom?)
(define (atom? x)
  (not (or  
        (pair? x)
        (null? x))))

(#%provide map-tree)
(define (map-tree f tree)
  (if (atom? tree)
      (f tree)
      (map (λ (t) (map-tree f t))
           tree)))

(#%provide from-to)
(define (from-to a b)
  "Return the list of integers in the range [a,b)"
  (define (iter curlist curval)
    (if (>= curval a)
        (iter (cons curval curlist) (dec curval))
        curlist))
  (iter nil (dec b)))

(#%provide from-to-by)
(define (from-to-by a b step)
  (if (> a b)
      nil
      (cons a
            (from-to-by (+ a step) b step))))


(#%provide square)
(define (square x) (* x x))

(#%provide logx-basey)
(define (logx-basey x y) (/ (log x) (log y)))
(#%provide log2)
(define (log2 x) (logx-basey x 2))
(#%provide log10)
(define (log10 x) (logx-basey x 10))

(#%provide exp2)
(define (exp2 x) (expt 2 x))
(#%provide exp10)
(define (exp10 x) (expt 10 x))
