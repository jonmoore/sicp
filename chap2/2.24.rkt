#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(module+ main
  (begin-example "2.24")
  (displn (list 1 (list 2 (list 3 4)))))

;; box and pointer structure has three lists each of length two
;; connected by the second element of two of the lists.

(module+ main
  (begin-example "2.25")
  (displn-eval '(cadr (caddr '(1 3 (5 7) 9))) ns)
  (displn-eval '(caar '((7))) ns)
  (displn-eval '(cadr (cadr (cadr (cadr (cadr (cadr '(1 (2 (3 (4 (5 (6 7)))))))))))) ns))

(define x (list 1 2 3))
(define y (list 4 5 6))
(module+ main
  (begin-example "2.26")
  (displn-eval '(append x y) ns)
  (displn-eval '(cons x y) ns)
  (displn-eval '(list x y) ns))

(define (deep-reverse lis)
  (define (process-car car-from)
    (if (atom? car-from)
        car-from
        (deep-reverse car-from)))
  (define (iter from to)
    (if (null? from)
        to
        (iter (cdr from) (cons (process-car (car from)) to))))
  (iter lis nil))


(module+ main
  (begin-example "2.27")
  (displn-eval '(deep-reverse (list (list 1 2) (list 3 4))) ns))

(define (fringe tree)
  (cond
   ((null? tree) nil)
   ((atom? tree) (list tree))
   (else (apply append (map fringe tree)))))

(define x-28 (list (list 1 2) (list 3 4)))
(module+ main
  (begin-example "2.28")
  (displn-eval '(fringe x-28) ns)
  (displn-eval '(fringe (list x-28 x-28)) ns))

;; 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (branches mobile)
  (list (left-branch mobile) (right-branch mobile)))

(define (total-weight mobile)
  (sum (map
        (lambda (branch)
          (structure-weight (branch-structure branch)))
        (branches mobile))))

(define (structure-weight structure)
  (if (atom? structure)
      structure
      (+ (total-weight structure))))

(module+ main
  (begin-example "2.29")
  (let ((mob (make-mobile (make-branch 3 1)
                          (make-branch 3 (make-mobile (make-branch 1 1)
                                                      (make-branch 1 1))))))
    (displn (total-weight mob))))

(define (branch-torque branch)
  (* (branch-length branch)
     (structure-weight (branch-structure branch))))

(define (mobile-balanced? mobile)
  (and (structure-balanced? (branch-structure (left-branch mobile)))
       (structure-balanced? (branch-structure (right-branch mobile)))
       (= (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile)))))

(define (structure-balanced? structure)
  (if (atom? structure)
      #t
      (mobile-balanced? structure)))

