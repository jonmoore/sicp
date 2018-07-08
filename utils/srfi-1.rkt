#lang sicp

(#%require (only racket module+) rackunit)

;; subset of https://srfi.schemers.org/srfi-1/srfi-1.html

;;; https://srfi.schemers.org/srfi-1/srfi-1.html#Constructors

;; https://srfi.schemers.org/srfi-1/srfi-1.html#iota
(#%provide iota)
(define (iota count . args)
  (define start
    (if (>= (length args) 1)
        (car args)
        0))
  (define step
    (if (>= (length args) 2)
        (cadr args)
        1))
  (define (iter cur-len cur-list)
    (if (= cur-len count)
        cur-list
        (iter (+ cur-len 1)
              (cons (+ start
                       (* step (- count cur-len 1)))
                    cur-list))))
  (if (< count 0)
      (error "count must be positive")
      (iter 0 '())))

(module+ test
  (test-case
   "check (iota 5)"
   (check-equal? (iota 5) '(0 1 2 3 4)))

  (test-case
   "check iota lengths"
   (for-each
    (lambda (n)
      (check-equal? (length (iota n)) n)
      (check-equal? (length (iota n -5)) n)
      (check-equal? (length (iota n 7 3)) n))
    (iota 5))))

;;; https://srfi.schemers.org/srfi-1/srfi-1.html#Selectors

;; https://srfi.schemers.org/srfi-1/srfi-1.html#list-ref
;; a safe version of list-ref
(define (-list-ref-safe clist i default)
  (if (< i 0) (error "i must be positive. received " i))
  (define (iter clist i)
    (cond ((null? clist) default)
          ((= i 0) (car clist))
          (else (iter (cdr clist) (dec i)))))
  (iter clist i))

(module+ test
  (test-case
   "-list-ref-safe with iota"
   (define (expected-for-iota len i default)
     (if (and (>= i 0) (< i len))
         i
         default))

   (for-each
    (lambda (len)
      (for-each
       (lambda (index)
         (with-check-info
          (('len len) ('index index))
          (check-equal?
           (-list-ref-safe (iota len) index #f)
           (expected-for-iota len index #f))))
         (iota (inc len))))
    (iota 5))))

(define (-take-reversed x i)
  (define (iter accum list to-take)
    (if (= to-take 0)
        accum
        (iter (cons (car list) accum) (cdr list) (dec to-take))))
  (iter '() x i))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#take
(#%provide take)
(define (take x i)
   (reverse (-take-reversed x i)))

(module+ test
  (test-case
   "take"
   (for-each
    (lambda (n)
      (check-equal?
       (take (iota 10) n)
       (iota n)))
    (iota 10))))

(define (-times f x i)
  ;; Applies f i times to x
  (if (= i 0)
      x
      (-times f (f x) (dec i))))

(module+ test
  (test-case
   "-times inc"
   (for-each
    (lambda (n)
      (check-equal? (-times inc 2 n) (+ 2 n)))
    (iota 5))))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#drop
(#%provide drop)
(define (drop x i)
  (-times cdr x i))

(module+ test
  (test-case
   "drop"
   (for-each
    (lambda (n)
      (check-equal? (drop (iota 10) n) (iota (- 10 n) n)))
    (iota 10))))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#take-right
(#%provide take-right)
(define (take-right x i)
  (drop x (- (length x) i)))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#drop-right
(#%provide drop-right)
(define (drop-right x i)
  (reverse
   (-take-reversed x (- (length x) i))))

;;; https://srfi.schemers.org/srfi-1/srfi-1.html#Miscellaneous

;; https://srfi.schemers.org/srfi-1/srfi-1.html#append
(define (append a b)
  (if (null? a)
      b
      (cons (car a)
            (append (cdr a) b))))

(module+ test
  (define (partition-check left right list)
    (for-each
     (lambda (n)
       (check-equal?
        (append (take list n) (drop list n))
        list))
     (iota (length list))))

  (test-case
   "append/take/drop"
   (partition-check take drop (iota 10)))
  (test-case
   "append/drop-right/take-right"
   (partition-check drop-right take-right (iota 10))))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#reverse
(#%provide reverse)
(define (reverse l)
  (fold cons '() l))

(module+ test
  (test-case
   "reversing iotas"
   (for-each
    (lambda (n)
      (check-equal?
       (reverse (iota n))
       (iota n (dec n) -1)))
    (iota 5))))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#zip
(#%provide zip)
(define (zip . lists)
  (apply srfi-map list lists))

(module+ test
  (define (sample-matrix nrows ncols)
    (map
     (lambda (row)
       (map
        (lambda (col)
          (+ (* 10 row) col))
        (iota ncols)))
     (iota nrows)))
  (test-case
   "repeated zip is identity"
   (for-each
    (lambda (nrows)
      (for-each
       (lambda (ncols)
         (with-check-info
          (('nrows nrows)
           ('ncols ncols))
          (let ((M (sample-matrix nrows ncols)))
            (check-equal? (apply zip (apply zip M)) M))))
       (iota 5 1)))
    (iota 5 1)))
  
  (test-case
   "zip unequal lengths 2"
   (for-each
    (lambda (len)
      (check-equal?
       (length (iota len))
       (length (zip (iota len) (iota (inc len))))))
    (iota 5)))
  (test-case
   "zip unequal lengths 3"
   (for-each
    (lambda (len)
      (check-equal?
       (length (iota len))
       (length (zip (iota (inc len)) (iota len)))))
    (iota 5))))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#count
(#%provide count)
(define (count pred clist)
  (fold (lambda (el prev-count)
          (if (pred el)
              (inc prev-count)
              prev-count))
        0
        clist))

(module+ test
  (test-case
   "counts with iota"
   (for-each
    (lambda (n)
      (check-equal?
       (count (lambda (el) #t) (iota n))
       n))
    (iota 10)))
  (test-case
   "counting odd numbers"
   (for-each
    (lambda (n)
      (check-equal?
       (count odd? (iota (* 2 n)))
       n))
    (iota 10))))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#FoldUnfoldMap

;; https://srfi.schemers.org/srfi-1/srfi-1.html#map
(#%provide srfi-map)
(define (srfi-map proc . lists)
  (unfold
   (lambda (lists) (any null? lists))
   (lambda (lists) (apply proc (map car lists)))
   (lambda (lists) (map cdr lists))
   lists))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#fold 
(#%provide fold)
(define (fold kcons knil list)
  "Single-list version for now"
  (if (null? list)
      knil
      (fold kcons (kcons (car list) knil) (cdr list))))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#fold-right 
(#%provide fold-right)
(define (fold-right kcons knil . lists)
  (define (recur lists)
    (if (any null? lists)
        knil
        (let ((heads (map car lists))
              (tails (map cdr lists)))
          (apply kcons (append
                        heads
                        (list (recur tails)))))))
  (recur lists))

;; fold-right kons knil clist1 clist2 ... -> value
;; The fundamental list recursion operator.
;; First, consider the single list-parameter case. If clist1 = (e1 e2 ... en), then this procedure returns

;; (fold-right kons knil lis) = (kons (car lis) (fold-right kons knil (cdr lis)))
;; (fold-right kons knil '()) = knil

;; pair-fold-right kons knil clist1 clist2 ... -> value
;; Holds the same relationship with fold-right that pair-fold holds with fold. Obeys the recursion
;; (pair-fold-right kons knil lis) = (kons lis (pair-fold-right kons knil (cdr lis)))
;; (pair-fold-right kons knil '()) = knil

;; https://srfi.schemers.org/srfi-1/srfi-1.html#pair-fold-right 
(#%provide pair-fold-right)
(define (pair-fold-right kcons knil . lists)
  (define (recur lists)
    (if (any null? lists)
        knil
        (apply kcons (append
                      lists
                      (list (recur (map cdr lists)))))))
  (recur lists))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#unfold
(#%provide unfold)
(define (unfold p f g seed . args)
  (define tail-gen (-list-ref-safe args 0 (lambda (x) '())))
  (define (recur seed)
    (if (p seed)
        (tail-gen seed)
        (cons (f seed)
              (recur (g seed)))))
  (recur seed))

(module+ test
  (test-case
   "unfold as list copying"
   (for-each
    (lambda (lis)
      (with-check-info
       (('lis lis))
       (check-equal?
        (unfold null? car cdr lis)
        lis)))
    (map iota (iota 5))))

  (test-case
   "unfold as append"
   (for-each
    (lambda (head)
      (with-check-info
       (('head head))
       (let ((tail '(#t 23)))
         (check-equal?
          (unfold null? car cdr head (lambda (x) tail))
          (append head tail)))))
    (map iota (iota 5)))))

;;; https://srfi.schemers.org/srfi-1/srfi-1.html#FilteringPartitioning
;; https://srfi.schemers.org/srfi-1/srfi-1.html#filter
(#%provide filter)
(define (filter pred list)
  (cond ((null? list) nil)
        ((pred (car list))
         (cons (car list)
               (filter pred (cdr list))))
        (else (filter pred (cdr list)))))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#partition
(#%provide partition)
(define (partition pred list)
  (if (null? list)
      (values nil nil)
      (call-with-values
          (lambda ()
            (partition pred (cdr list)))
        (lambda (in-rest out-rest)
          (if (pred (car list))
              (values (cons (car list) in-rest) out-rest)
              (values in-rest (cons (car list) out-rest)))))))

(module+ test
  (test-case
   "filter on empty list"
   (check-equal?
    (filter odd? '())
    '()))

  (test-case
   "filter with odd?"
   (for-each
    (lambda (n)
      (check-equal?
       (filter odd? (iota (* 2 n)))
       (iota n 1 2)))
    (iota 10))))

;;; https://srfi.schemers.org/srfi-1/srfi-1.html#Searching
;; https://srfi.schemers.org/srfi-1/srfi-1.html#any
(#%provide any)
(define (any pred . lists)
  (define (any-iter . lists)
    (if (apply srfi-or-proc (map null? lists))
        #f
        (or (apply pred (map car lists))
            (apply any-iter (map cdr lists)))))
  (apply any-iter lists))

(module+ test
  (test-case
   "testing any"
   (check-equal? (any (lambda (x) #t) '())
                 #f)
   (check-equal? (any values '(2 1))
                 2)
   (check-equal? (any values '(#f 2 1))
                 2)
   (check-equal? (any values '(#t 2 1))
                 #t)))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#every
(#%provide every)
(define (every pred . lists)
  (define (every-iter last-value . lists)
    (if (apply srfi-or-proc (map null? lists))
        last-value
        (let ((next-value (apply pred (map car lists))))
          (if (not next-value)
              #f
              (apply every-iter
                     (cons next-value
                           (map cdr lists)))))))
  (apply every-iter (cons #t lists)))

(module+ test
  (test-case
   "testing every"
   (check-equal? (every (lambda (x) #f) '())
                 #t)
   (check-equal? (every values '(2 1))
                 1)
   (check-equal? (every values '(2 #f 1))
                 #f)))

;; https://srfi.schemers.org/srfi-1/srfi-1.html#take-while
(#%provide take-while)
(define (take-while pred clist)
  (if (null? clist)
      '()
      (if (pred (car clist))
          (cons (car clist)
                (take-while pred (cdr clist)))
          '())))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (srfi-or-proc . args)
  "To replace or whenever we need a procedure.  This is
iterative and short-circuited."
  
  (cond
   ((null? args) #f)
   ((car args) (car args))
   (else (apply srfi-or-proc (cdr args)))))

