#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt" "../chap1/ex1.21.rkt")
(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define (flatmap proc seq)
  (fold-right append nil (map proc seq)))

(define (unique-pairs n)
  ;; generates the sequence of pairs (i,j) with 1≤j<i≤n
  (flatmap
   (lambda (i)
     (map (lambda (j) (list j i)) (iota (dec i) 1)))
   (iota n 1)))
 ;;
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) 
        (cadr pair) 
        (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter 
        prime-sum?
        (unique-pairs n))))

(module+ main
  (begin-example "2.40")
  (displn-eval '(unique-pairs 4) ns)
  (displn-eval '(prime-sum-pairs 7) ns))

(define (ordered-triples n)
  ;; triples of distinct positive integers i, j, and k less than or
  ;; equal to n
  (flatmap
   (lambda (k) (map (lambda (pair) (append pair (list k))) (unique-pairs (dec k))))
   (iota n 1)))

(define (triples-adding-to n s)
  (filter
   (lambda (triple)
     (= (sum triple) s))
   (ordered-triples n)))
   
(module+ main
  (begin-example "2.41")
  (displn-eval '(ordered-triples 6) ns)
  (displn-eval '(triples-adding-to 8 14) ns))

(define (enumerate-interval a b)
  (iota (inc (- b a)) a))

(define (queens board-size)
  (define (queen-cols k)
    ;; Returns the sequence of all ways to place queens in the first k columns of the board.
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k 
                                    rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; requires empty-board safe? adjoin-position 

(define empty-board nil)
(define (pos row col) (cons row col))
(define (pos-row pos) (car pos))
(define (pos-col pos) (cdr pos))
(define (adjoin-position new-row k rest-of-queens)
  (cons (pos new-row k) rest-of-queens))
(define (safe? k positions)
  (define (conflicting-pair? pos1 pos2)
    (or (= (pos-row pos1) (pos-row pos2))
        (= (abs (- (pos-col pos2) (pos-col pos1)))
           (abs (- (pos-row pos2) (pos-row pos1))))))
  (define (conflicts new-pos other-queens)
    (filter
     (lambda (pos2) (conflicting-pair? new-pos pos2))
     other-queens))
  (null? (conflicts (car positions) (cdr positions))))
    
(module+ main
  (begin-example "2.42")
  (displn-eval '(enumerate-interval 1 8) ns)
  ;; Expect 92 solutions for n = 8
  ;; http://www.ic-net.or.jp/home/takaken/e/queen/
  ;; https://en.wikipedia.org/wiki/Eight_queens_puzzle
  (displn-eval '(length (queens 8)) ns))

;; 2.43 One issue is that Louis is recalculating (queen-cols (- k 1))
;; for each of the board-size rows in order to calculate (queen-cols
;; k).


