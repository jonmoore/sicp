#lang sicp

(#%provide sum-list)
(define (sum-list list)
  (fold-left + 0 list))

(#%provide average)
(define (average list)
  (/ (sum-list list) (length list)))

(define (and-proc . args)
  "A procedure to replace and whenever we need a procedure,
e.g. calling apply. This is iterative and short-circuited."
  (define (and-proc-inner . args)
    "and, but args must be non-null"
    (if (null? (cdr args))
        (car args)
        (if (car args)
            (apply and-proc-inner (cdr args))
            #f)))
  (or (null? args)
      (apply and-proc-inner args)))

(#%provide any)
(define (any pred . lists)
  "A local version of Scheme's any.  Mainly written as an exercise,
since we should be able to import the Scheme version."
  (define (any-iter . lists)
    (if (apply or-proc (map null? lists))
        #f
        (or (apply pred (map car lists))
            (apply any-iter (map cdr lists)))))
  (apply any-iter lists))


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


