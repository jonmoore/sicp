#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (app x y)
  ;; non-mutating append
  (if (null? x)
      y
      (cons (car x) (app (cdr x) y))))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))


(define (app! x y)
  ;; mutating append
  (set-cdr! (last-pair x) y)
  x)

(module+ test
  (begin-example "3.12, difference in append and append!")
  
  (test-case
   "difference in append and append!"
   
   (define x (list 'a 'b))
   (define y (list 'c 'd))
   (define z (app x y))
   
   ;; no change in (cdr x) after using app
   (check-equal? (cdr x) '(b))
   
   (app! x y) ;; for side-effect
   
   ;; contents of y now spliced onto the end of (cdr x)
   (check-equal? (cdr x) '(b c d))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(module+ test
  (begin-example "3.13, make-cycle")

  (define z (make-cycle (list 'a 'b 'c)))
  ;; z will not be a list but will have a cycle with the pointer to
  ;; null in x replaced by a pointer to the head of the list.  x will
  ;; point to the same cycle

  ;; if we try to compute (last-pair z) we will trigger an infinite
  ;; recursion since the null? condition to terminate the recursion
  ;; will never be met.  Tail-call optimization in Scheme suggests
  ;; that we're likely to run an infinite loop rather than exhaust
  ;; stack or other memory.
  
  )

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; What does this do?  On each loop with x not null
;;
;; x <- (cdr x)
;; y <- (cons (car x) y)
;;

;; Thus we push the elements of x onto y until temp is null, i.e. (cdr
;; x) in the parent call is null

;; Operations on x are done in-place.  if x is non-null initially then
;; it is mutated to a singleton; since set-cdr! is the only mutation,
;; the car of the original x is untouched throughout and it is never
;; set to null.

;; If x is null initially then we return null and x is unchanged.
;; 
;; Overall, mystery will return the reverse of v, acting destructively
;; on v.


(module+ test
  (begin-example "3.14, mystery")

  (test-case
   "mystery on non-empty list"
   (define v (list 'a 'b 'c 'd))
   (define w (mystery v))
   (check-equal? w (list 'd 'c 'b 'a))
   (check-equal? v (list 'a)))

  (test-case
   "mystery on empty-list"
   (check-equal? '() (mystery '()))))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

;;  "Even though z1 and z2 are "the same" structure, applying
;;  set-to-wow! to them yields different results. With z1, altering
;;  the car also changes the cdr, because in z1 the car and the cdr
;;  are the same pair. With z2, the car and cdr are distinct, so
;;  set-to-wow! modifies only the car:"

(begin-example "3.15, list mutation")

(module+ main
  (let* ((x '(a b))
         (z1 (list x x))
         (z2 (cons '(a b) '(a b))))
    (displn 
     (list (set-to-wow! z1)
           (set-to-wow! z2)))))

(begin-example "3.16, counting pairs")

;; Exercise 3.16: Ben Bitdiddle decides to write a procedure to
;; count the number of pairs in any list structure. “It’s easy,” he
;; reasons. “The number of pairs in any structure is the number in
;; the car plus the number in the cdr plus one more to count the
;; current pair.” So Ben writes the following procedure:

(define (count-pairs x)
  (if (not (pair? x))
      0
      (+ (count-pairs (car x))
         (count-pairs (cdr x))
         1)))

;; Show that this procedure is not correct. In particular, draw
;; box-and-pointer diagrams representing list structures made up of
;; exactly three pairs for which Ben’s procedure would return 3;
;; return 4; return 7; never return at all.

(define (case-3)
  (let* ((p1 (cons 1 1))
         (p2 (cons p1 1))
         (p3 (cons p2 1)))
    p3))
(define (case-4)
  (let* ((p1 (cons 1 1))
         (p2 (cons p1 p1))
         (p3 (cons p2 1)))
    p3))
(define (case-7)
  (let* ((p1 (cons 1 1))
         (p2 (cons p1 p1))
         (p3 (cons p2 p2)))
    p3))

(module+ test
  (test-case "testing count-pairs with 3 pairs"
   (check-equal? (count-pairs (case-3)) 3)
   (check-equal? (count-pairs (case-4)) 4)
   (check-equal? (count-pairs (case-7)) 7))
   ;; non-terminating case - will need to construct a cycle.  Easiest
   ;; way is above but mutate the first pair we construct
  )

(begin-example "3.17, correcting count-pairs")
;; Exercise 3.17: Devise a correct version of the count-pairs
;; procedure of Exercise 3.16 that returns the number of distinct
;; pairs in any structure. (Hint: Traverse the structure, maintaining
;; an auxiliary data structure that is used to keep track of which
;; pairs have already been counted.)

(define (count-pairs-2 p)
  (define (collect-pairs p2 known-pairs)
    (if (or (not (pair? p2))
            (memq p2 known-pairs))
        known-pairs
        (collect-pairs
         (car p2)
         (collect-pairs
          (cdr p2)
          (cons p2 known-pairs)))))
  (let ((all-pairs (collect-pairs p '())))
    (length all-pairs)))

(module+ test
  (test-case "testing count-pairs-2 with 3 pairs"
   (check-equal? (count-pairs-2 (case-3)) 3)
   (check-equal? (count-pairs-2 (case-4)) 3)
   (check-equal? (count-pairs-2 (case-7)) 3))
  )

(begin-example "3.18, cycle detection")
;; Exercise 3.18: Write a procedure that examines a list and
;; determines whether it contains a cycle, that is, whether a program
;; that tried to find the end of the list by taking successive cdrs
;; would go into an infinite loop. Exercise 3.13 constructed such
;; lists.

;; passed in favor of 3.19

(begin-example "3.19, cycle detection")

;; Exercise 3.19: Redo Exercise 3.18 using an algorithm that takes
;; only a constant amount of space

(define (has-cycle l)
  ;; idea - hare starts 1 ahead of tortoise. at each iteration if hare
  ;; == tortoise there's a cycle. if hare cannot advance there's not.
  ;; otherwise advance and try again
  
  (define (iter hare tortoise)
    (cond
     ((eq? hare tortoise) #t)
     ((not (pair? hare)) #f)
     ((not (pair? (cdr hare))) #f)
     (else (iter (cddr hare) (cdr tortoise)))))
  (if (pair? l)
      (iter (cdr l) l)
      #f))

(define (make-n-cycle n)
  (if (> n 0)
      (make-cycle (iota n))
      (error "n must be greater than 0")))
  
(module+ test
  (test-case
   "Lengths in [0, 4]"
   (for-each
    (lambda (n)
      (with-check-info (('n n))
                       (check-equal? (has-cycle (iota n)) #f)
                       (if (> n 0)
                           (check-equal? (has-cycle (make-n-cycle n)) #t))))
    (iota 4))))

(begin-example "3.20, mutation and assignment")

;; key point is that each time cons is called, we get a lambda with
;; its own environment

