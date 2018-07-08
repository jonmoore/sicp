#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit racket/trace)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))
(define (just x) (adjoin-set x '()))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))
        

(module+ main
  (begin-example "2.63")
  (displn-eval
   '(let ((t1 (make-tree 7
                         (make-tree 3 (just 1) (just 5))
                         (make-tree 9 '() (just 11))))
          (t2 (make-tree 3
                         (just 1)
                         (make-tree
                          7
                          (just 5)
                          (make-tree 9
                                     '()
                                     (just 11)))))
          (t3 (make-tree 5
                         (make-tree 3 (just 1) '())
                         (make-tree 9 (just 7) (just 11)))))
      (map
       (lambda (tree)
         (list (tree->list-1 tree) (tree->list-2 tree)))
       (list t1 t2 t3)))
   ns))

;; a. tree->list-1 and tree->list-2 do produce the same result for every treee
;;
;; b. costs
;;
;; Work from scratch
;; 
;; version 2
;; C(n) ~= 2*C(n/2)+O(1), so O(n)
;;
;; version 1
;; C(n) ~= 2*C(n/2)+O(n/2)+1, because of copying needed for append
;;
;; Try: C(n) = a * n log n + b n
;; 2*( a n/2 log n - a n/2 log 2 + bn/2) + cn
;;
;; equate for n log n
;; a = a
;; equate for n
;; b = -a log 2 + b + c
;;
;; Master Theorem check
;;
;; T(n) = a T(n / b) + f(n)
;;
;; here a = 2, b = 2
;; f = dn+e = O(n^c) with c = 1 
;;
;; c_crit = log_b a = 1
;;
;; so f(n) = O (n^{c_crit} log^k n) with k = 0
;;
;; => T(n) = O(n^{c_crit} log^{k+1} n)
;; => in this case n log n

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  ;; Returns a pair whose car is a balanced tree formed from the first
  ;; n elements of elts and whose cdr is the list of elements not
  ;; included in the tree
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons
                 (make-tree this-entry left-tree right-tree)
                 remaining-elts))))))))

(define (partial-tree-2 elts n)
  ;; Returns a pair whose car is a balanced tree formed from the first
  ;; n elements of elts and whose cdr is the list of elements not
  ;; included in the tree
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (partial-tree-2 elts left-size))
             (non-left-elts (cdr left-result))
             (this-entry (car non-left-elts))

             (left-tree (car left-result))

             (right-size (- n (+ left-size 1)))
             (right-result (partial-tree-2 (cdr non-left-elts) right-size))
             (right-tree (car right-result))

             (remaining-elts (cdr right-result)))
        (cons
         (make-tree this-entry left-tree right-tree)
         remaining-elts))))

;; 2.64a
;; if n is odd, two equal-sized sub-trees are produced
;; if n is even, >0 the right tree has one more element
;; (list->tree (1 3 5 7 9 11)) 
;; -> (make-tree 5
;;       (make-tree 1 nil (just 3)
;;       (make-tree 9 (just 7) (just 11)))

;; 2.64b
;;
;; O(n) - all-ops are  O(1) except the two calls to partial-tree with ~= n /2 elements

(module+ main
  (begin-example "2.64")
  (displn-eval '(list->tree (list 1 3 5 7 9 11)) ns))

;; This is just union-set from the ordered set representation
(define (union-list list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else
         (let ((x1 (car list1))
               (x2 (car list2)))
           (cond ((= x1 x2)
                  (cons x1 (union-list (cdr list1) (cdr list2))))
                 ((< x1 x2)
                  (cons x1 (union-list (cdr list1) list2)))
                 ((< x2 x1)
                  (cons x2 (union-list list1 (cdr list2)))))))))

(define (intersection-list list1 list2)
  ;; This is just intersection-set from 2.61 relabeled
  (if ((or (null? list1) (null? list2))
       '())
      (let ((x1 (car list1))
            (x2 (car list2)))
        (cond ((= x1 x2)
               (cons x1 (intersection-list (cdr list1) (cdr list2))))
              ((< x1 x2)
               (intersection-list (cdr list1) list2))
              ((< x2 x1)
               (intersection-list list1 (cdr list2)))))))

(define (union-set set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (list->tree (union-list list1 list2))))

(define (intersection-set set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (list->tree (intersection-list list1 list2))))

(module+ main
  (begin-example "2.65")
  (displn-eval '(tree->list-2 (union-set (list->tree (iota 5 0 2))
                                         (list->tree (iota 10 3 1))))
               ns))


(define (lookup wanted-key records)
  ;; based on element-of-set?
  ;; assumes the existence of a record-key function for an element
  (cond ((null? records) (error "key not found" wanted-key))
        (else (let ((cur-key (record-key (entry records))))
                (cond 
                 ((= wanted-key cur-key) (entry records))
                 ((< wanted-key cur-key)
                  (lookup wanted-key (left-branch records)))
                 ((> wanted-key cur-key)
                  (lookup wanted-key (right-branch records))))))))
