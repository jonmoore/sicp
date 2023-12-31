#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(begin-example "Section 3.3.2, Representing Queues") 

;; Implementation of queues from the text

(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item)
  (set-car! queue item))
(define (set-rear-ptr! queue item)
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  ;; select the item at the front of the queue
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  ;; insert an item in a queue, return the queue
  (let ((new-pair (cons item '())))
    (cond 
     ((empty-queue? queue)
      (set-front-ptr! queue new-pair)
      (set-rear-ptr! queue new-pair)
      queue)
     (else
      (set-cdr! (rear-ptr queue) new-pair)
      (set-rear-ptr! queue new-pair)
      queue))))

(define (delete-queue! queue)
  ;; delete the item at the front of the queue, and return the
  ;; resulting queue
  (cond ((empty-queue? queue)
         (error "DELETE called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

(module+ test
  (begin-example "3.21, queue operations") 
  (test-case
   "basic queue operations"

   (define q1 (make-queue))

   (insert-queue! q1 'a)
   ;; queue holds both front-ptr and rear-ptr and is structured as a
   ;; 2-element list where the first element is the list accessible
   ;; via front-ptr and the second is the element pointed to by
   ;; rear-ptr, if there is one.
   (check-equal? q1 '((a) a))

   (insert-queue! q1 'b)
   (check-equal? q1 '((a b) b))

   (delete-queue! q1)
   (check-equal? q1 '((b) b))

   (delete-queue! q1)
   ;; b is still recorded since it is accessible by rear-ptr and we
   ;; don't care what rear-ptr is when front-ptr is null -- our queue
   ;; is always either an empty queue (defined by front-ptr being
   ;; null) or else a non-empty one.
   (check-equal? q1 '(() b))
   ))

(module+ main
  (define q1 (make-queue))

  (define (print-queue queue)
    (display "queue: ")
    (displn (front-ptr queue)))

  (insert-queue! q1 'a)
  (print-queue q1)

  (insert-queue! q1 'b)
  (print-queue q1)

  (delete-queue! q1)
  (print-queue q1)

  (delete-queue! q1)
  (print-queue q1))

;; Exercise 3.22: Instead of representing a queue as a pair of
;; pointers, we can build a queue as a procedure with local state. The
;; local state will consist of pointers to the beginning and the end
;; of an ordinary list ... Complete the definition of make-queue and
;; provide implementations of the queue operations using this
;; representation.

;; use "-q" instead of "-queue" to avoid clashes
(define (make-q)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty?)
      (null? front-ptr))
    (define (dispatch m)
      (cond
       ((eq? m 'empty-q?)
        (empty?))
       ((eq? m 'front-q)
        (if (empty?)
            (error "FRONT called with empty queue")
            (car front-ptr)))
       ((eq? m 'insert-q!)
        (lambda (item)
          (let ((new-pair (cons item '())))
            (cond 
             ((empty?)
              (set! front-ptr new-pair)
              (set! rear-ptr new-pair)
              dispatch)
             (else
              (set-cdr! rear-ptr new-pair)
              (set! rear-ptr new-pair)
              dispatch)))))
       ((eq? m 'delete-q!)
        (cond
         ((empty?)
          (error "DELETE called with an empty queue"))
         (else
          (set! front-ptr (cdr front-ptr))
          dispatch)))
       (else
        (error "DISPATCH called with unrecognized message" m))))
    dispatch))

(define (empty-q? q) (q 'empty-q?))
(define (front-q q) (q 'front-q))
(define (insert-q! q item) ((q 'insert-q!) item))
(define (delete-q! q) (q 'delete-q!))

(module+ test
  (begin-example "3.22, queue as procedure with local state")

  ;; same tests as before
  (test-case
   "basic queue operations v2"
   (define q1 (make-q))
   
   (insert-q! q1 'a)
   (check-equal? (front-q q1) 'a)

   (insert-q! q1 'b)
   (check-equal? (front-q q1) 'a)

   (delete-q! q1)
   (check-equal? (front-q q1) 'b)

   (delete-q! q1)
   (check-equal? (empty-q? q1) #t)
   ))

;; Exercise 3.23: A deque (“double-ended queue”) is a sequence in
;; which items can be inserted and deleted at either the front or the
;; rear. Operations on deques are the constructor make-deque, the
;; predicate empty-deque?, selectors front-deque and rear-deque, and
;; mutators front-insert-deque!, rear-insert-deque!,
;; front-delete-deque!, rear-delete-deque!. Show how to represent
;; deques using pairs, and give implementations of the operations.
;; All operations should be accomplished in Θ(1) steps.

;; queue operations

(define (make-node value dummy)
  (define next nil)
  (define prev nil)
  (define (dispatch m)
    (cond
     ((eq? m 'node-set-next!)
      (lambda (item)
        (set! next item)))
     ((eq? m 'node-set-prev!)
      (lambda (item)
        (set! prev item)))
     ((eq? m 'node-dummy)
      dummy)
     ((eq? m 'node-value)
      (if dummy
          (error "NODE-VALUE -- trying to get value of dummy node")
          value))
     ((eq? m 'node-next)
      next)
     ((eq? m 'node-prev)
      prev)
     (else
      (error "NODE-DISPATCH unrecognized message" m))))
  dispatch)
(define (node-set-next! node item) ((node 'node-set-next!) item))
(define (node-set-prev! node item) ((node 'node-set-prev!) item))
(define (node-value node) (node 'node-value))
(define (node-dummy node) (node 'node-dummy))
(define (node-next node) (node 'node-next))
(define (node-prev node) (node 'node-prev))
(define (make-real-node value) (make-node value #f))
;; idea
;; fp                        rp
;; nil - e1 - e2 - ... en - nil

(define (make-deque)
  (define begin-node (make-node "BEGIN-NODE" #t))
  (define end-node (make-node "END-NODE" #t))
  (define (front-node)
    (node-next begin-node))
  (define (rear-node)
    (node-prev end-node))
  (define (empty?)
    (node-dummy (front-node)))
  (define (insert-between new-node before after)
    (node-set-prev! new-node before)
    (node-set-next! new-node after)
    (node-set-prev! after new-node)
    (node-set-next! before new-node))
  (define (node-join prev next)
    (node-set-prev! next prev)
    (node-set-next! prev next))
  (define (dispatch m)
    (cond
     ((eq? m 'empty-deque?)
      (empty?))
     ((eq? m 'front-insert-deque!)
      (lambda (item)
        (insert-between (make-real-node item) begin-node (front-node))
        dispatch))
     ((eq? m 'front-deque)
      (node-value (front-node)))
     ((eq? m 'front-delete-deque!)
      (node-join begin-node (node-next (front-node)))
      dispatch)
     ((eq? m 'rear-insert-deque!)
      (lambda (item)
        (insert-between (make-real-node item) (rear-node) end-node)
        dispatch))
     ((eq? m 'rear-deque)
      (node-value (rear-node)))
     ((eq? m 'rear-delete-deque!)
      (node-join (node-prev (rear-node)) end-node)
      dispatch)
     (else
      (error "DEQUE-DISPATCH unrecognized message" m))))
  (node-set-next! begin-node end-node)
  (node-set-prev! end-node begin-node)
  dispatch)
(define (empty-deque? deque) (deque 'empty-deque?))
(define (front-insert-deque! deque item) ((deque 'front-insert-deque!) item))
(define (front-deque deque) (deque 'front-deque))
(define (front-delete-deque! deque) (deque 'front-delete-deque!))

(define (rear-insert-deque! deque item) ((deque 'rear-insert-deque!) item))
(define (rear-deque deque) (deque 'rear-deque))
(define (rear-delete-deque! deque) (deque 'rear-delete-deque!))

(module+ test
  (begin-example "3.23, deque implementation")
  (test-case
   "deque operations"
   (define d1 (make-deque))
   (check-equal? (empty-deque? d1) #t)

   (front-insert-deque! d1 1)
   (check-equal? (empty-deque? d1) #f)
   (check-equal? (front-deque d1) 1)

   (front-insert-deque! d1 2)
   (check-equal? (front-deque d1) 2)

   (front-delete-deque! d1)
   (check-equal? (front-deque d1) 1)

   (front-delete-deque! d1)

   ))
