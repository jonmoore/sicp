#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit racket/trace)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(module+ test
  (begin-example "3.9 environment diagrams for factorial evaluation")
  (displn "TODO: drawing exercise"))


(define (make-withdraw initial-amount)
  (let ((balance initial-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance 
                   (- balance amount))
                 balance)
          "Insufficient funds"))))

(module+ test
  (begin-example "3.10 environment diagrams for evaluation with make-withdraw")

  ;; Use the environment model to analyze this alternate version of
  ;; make-withdraw, drawing figures like the ones above to illustrate
  ;; the interactions

  (displn "TODO: drawing exercise")

  (displn-eval 
   '(begin
      (define W1 (make-withdraw 100))
      (W1 50)
      (define W2 (make-withdraw 100)))
   ns))


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance 
                     (- balance 
                        amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: 
                        MAKE-ACCOUNT" 
                       m))))
  dispatch)

(module+ test
  (begin-example "3.11 environment diagrams make-account")

  ;; Show the environment structure generated by the sequence of interactions 
  (define acc (make-account 50))
  ((acc 'deposit) 40)
  ((acc 'withdraw) 60)
  
  ;; Where is the local state for acc kept?
  
  ;; Suppose we define another account

  (define acc2 (make-account 100))
  ;; How are the local states for the two accounts kept distinct?
  ;; Which parts of the environment structure are shared between acc
  ;; and acc2?
)