#lang sicp
(#%require (only racket module+ define-namespace-anchor namespace-anchor->namespace) rackunit racket/trace)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds"))

(module+ main
  (displn-eval '(withdraw 50) ns)
  (displn-eval '(withdraw 20) ns)
  )

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds"))))

(module+ main
  (displn-eval '(new-withdraw 50) ns)
  (displn-eval '(new-withdraw 20) ns)
  )


(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds")))

(module+ main
  (displn-eval '(let ((withdraw (make-withdraw 100)))
                  (withdraw 50)
                  (withdraw 20))
               ns))

(module+ main
  (displn-eval '(let ((w1 (make-withdraw 100))
                      (w2 (make-withdraw 100)))
                  (w1 50)
                  (w2 70)
                  (w1 20)
                  (w2 25))
               ns))


(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds!"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(module+ main
  (displn-eval '(let ((a1 (make-account 100)))
                  ((a1 'withdraw) 50)
                  ((a1 'deposit) 45))
               ns))


(define (make-accumulator init)
  (lambda (x)
    (set! init (+ init x))
    init))

(module+ main
  (displn-eval
   '(let ((a (make-accumulator 5)))
      (a 10)
      (a 10))
   ns))

(module+ test
  (begin-example "3.1, accumulator")

  (test-case
   "Basic accumulator"
   
   (let ((a (make-accumulator 5)))

     (check-equal? (a 10) 15)
     (check-equal? (a 10) 25))))

(define (make-monitored f)
  ;; f - function of one input
  ;;
  ;; returns - a procedure of one input that keeps track of how many
  ;; times it has been called.  If this is passed 'how-many-calls? it
  ;; returns the value of the counter; if it is passed 'reset-count it
  ;; resets the counter to 0; otherwise it passes through to f and
  ;; increments the counter
  
  (define counter 0)
  (define (mf x)
    (cond
     ((eq? x 'how-many-calls?) counter)
     ((eq? x 'reset-count) (set! counter 0))
     (else (begin
             (set! counter (inc counter))
             (f x)))))
  mf)

(module+ test
  (begin-example "3.2, make-monitored")
  (test-case
   "make-monitored"
   (let ((s (make-monitored sqrt)))
     (check-equal? (s 100) 10)
     (check-equal? (s 'how-many-calls?) 1))))


  
(define (make-protected-account balance account-password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds!"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
    (if (not (eq? password account-password))
        (error "wrong password"))
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(module+ test
  (begin-example "3.3, password-protected account")
  (test-case
   "password-protected account"
   (let ((pa1 (make-protected-account 100 'sesame))
         (pa2 (make-protected-account 100 'sesame)))
     (check-equal?
      ((pa1 'sesame 'withdraw) 40)
      60)
     (check-equal?
      ((pa1 'sesame 'withdraw) 40)
      20)
     (check-equal?
      ((pa2 'sesame 'withdraw) 40)
      60)
     
     (check-exn
      (lambda (exn) #t)
      (lambda ()
        ((pa1 'foo 'withdraw) 40))))))


(define (call-the-cops)
  "calling the cops")

(define (make-protected-account-with-limit balance account-password)
  (define bad-password-count 0)
  (define bad-password-limit 3)
  
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds!"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch password m)
    (if (not (eq? password account-password))
        (begin
          (set! bad-password-count (inc bad-password-count))
          (if (> bad-password-count bad-password-limit)
              (lambda (_) (call-the-cops))
              (lambda (_) "wrong password")))
        (begin 
          (set! bad-password-count 0)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                (else (error "Unknown request -- MAKE-ACCOUNT"
                             m))))))
  dispatch)

(module+ test
  (begin-example "3.4, cop-calling-password-protected account")
  (test-case
   "password-protected account"
   (let ((pa1 (make-protected-account-with-limit 100 'sesame))
         (pa2 (make-protected-account-with-limit 100 'sesame)))

     (check-equal? ((pa1 'sesame 'withdraw) 40) 60)
     
     (check-equal? ((pa1 'foo 'withdraw) 10) "wrong password")
     (check-equal? ((pa1 'foo 'withdraw) 10) "wrong password")
     (check-equal? ((pa1 'foo 'withdraw) 10) "wrong password")
     (check-equal? ((pa1 'foo 'withdraw) 10) "calling the cops")

     (check-equal? ((pa2 'sesame 'withdraw) 40) 60)
     )))

