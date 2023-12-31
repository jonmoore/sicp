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



(define (make-balance-holder balance)
  ;; we create an object to manage a balance
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
          (else (error "Unknown request -- MAKE-BALANCE"
                       m))))
  dispatch)

(define (make-protected-account initial-balance initial-password)
  ;; There is a single shared balance and multiple passwords can be
  ;; used to access it.  However the passwords are not shared -- to
  ;; access the account you need to use the password the access was
  ;; designated to have.
  (let ((balance-holder (make-balance-holder initial-balance)))
    (define (make-account-handler saved-password)
      (lambda (user-password m)
        (if (not (eq? user-password saved-password))
            (error "wrong password"))
        (if (eq? m 'add-password)
            add-password
            (balance-holder m))))
    (define (add-password new-password)
      (make-account-handler new-password))
    (make-account-handler initial-password)))

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


;; random-number generation
;; ========================
;;
;; Linear Congruential Sequences
;;
;; http://www.wilbaden.com/neil_bawd/lincongs.html, based on Knuth

;;
;; X[n+1] = (a*X[n] + c) mod m
;;

;; - m, the modulus; 0 < m. 

;; - a, the multiplier; 0 <= a < m. 

;; - c, the increment; 0 <= c < m. 

;; - X[0], the starting value; 0 <= X[0] < m. 

;;
;; i. The "seed" number X[0] may be chosen arbitrarily. 

;; ii. The modulus m should be large, say at least 2^30. Conveniently
;; it may be the computer's word size, since that makes the
;; computation quite efficient.

;; iii. If m is a power of 2, pick a so that a mod 8 is 5. If m is a
;; power of 10, choose a so that a mod 200 is 21.

;; iv. The multiplier a should preferably be chosen between .01*m and
;; .99*m, and its binary or decimal digits should not have a simple,
;; regular pattern ... Knuth recommends a "haphazard" constant like
;; 3_141_592_621.

;; v. The value of the increment c is immaterial when a is a good
;; multiplier, except that c must have no factor in common with m.  So
;; 1 looks like a good value for c.

;; vii. The randomness in t dimensions is only one part in the t-th
;; root of m.

;; viii. At most m/1000 numbers should be generated; otherwise the
;; future will behave more and more like the past.


;; "EASY-RAND was nominated by George Marsaglia, 1972, as a candidate
;; for the best multiplier, perhaps because 69069 is easy to remember."

;;    : EASY-RAND-NEXT  ( -- 0..4294967295 )
;;       RAND-X @
;;          69069 * 1+
;;       DUP RAND-NEXT ! ;

;; The above is in the stack-based language FORTH. The ":" begins the
;; definition of EASY-RAND-NEXT and the ";" ends it.  The "!" assigns,
;; binding the preceding symbol.
;;
;; It implements the recurrence 
;;
;; X[n+1] = (a*X[n] + c) mod m
;; 
;; with multiplier a as 69096, increment c as 1 and modulus m as 2^32.
;; the modulus operation is presumably implicitly applied because of
;; an assumption about the behavior of * in the FORTH runtime.
;;
;; Execution example, showing the stack evolution
;; X[0]
;; 69069 X[0]
;; (* 69069 X[0])
;; (modulo (+ (* 69069 X[0]) 1) 2^32)
;; ;; call this X[1]
;; X[1] X[1] ;; after dup
;; X[1] ;; after the dup'ed value is read/consumed

;; Other examples at the link use FORTH's */mod

;; http://lars.nocrew.org/forth2012/core/TimesDivMOD.html
;;
;; /MODstar-slash-mod
;;
;;   ( n1 n2 n3 -- n4 n5 )
;;
;; Multiply n1 by n2 producing the intermediate double-cell result
;; d. Divide d by n3 producing the single-cell remainder n4 and the
;; single-cell quotient n5. An ambiguous condition exists if n3 is
;; zero, or if the quotient n5 lies outside the range of a single-cell
;; signed integer. If d and n3 differ in sign, the
;; implementation-defined result returned will be the same as that
;; returned by either the phrase >R M* R> FM/MOD or the phrase >R M*
;; R> SM/REM.

;; (define random
;;   (let ((a 69069) (c 1) (m (expt 2 32)) (seed 19380110))
;;     (lambda new-seed
;;       (if (pair? new-seed)
;;           (set! seed (car new-seed))
;;           (set! seed (modulo (+ (* seed a) c) m)))
;;       (/ seed m))))

;; 
(define (make-rand random-init rand-update)
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (lcg a c m)
  (lambda (x)
    (modulo (+ (* x a) c) m)))

(define lcg-easy-rand
  (lcg 69069 1 (expt 2 32)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (random) (random)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (* 1.0 (/ trials-passed trials)))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define random-init 1)
(define easy-rand
  (make-rand random-init lcg-easy-rand))

(define (random-float-in-range low high)
  (let ((easy-rand-mod (expt 2 32))
        (sample (easy-rand))
        (range (- high low)))
    (+ low (* 1.0 range (/ sample easy-rand-mod)))))
  
(define (estimate-integral P x1 x2 y1 y2)
  (let ((integral-experiment
         (lambda ()
           (P (random-float-in-range x1 x2)
              (random-float-in-range y1 y2))))
        (area (* (- x2 x1) (- y2 y1))))
    (* (monte-carlo 1000 integral-experiment)
       area)))

(define (in-unit-circle x y) (< (+ (* x x) (* y y)) 1))

(module+ test
  (begin-example "3.5, Monte Carlo integral")
  (test-case
   "area of unit circle"
   (check-= (estimate-integral in-unit-circle -1 1 -1 1)
            (* 4 (atan 1.0)) ;; pi
            0.05))
  (test-case
   "integral of x*2 from 0 to 1"
   (check-= (estimate-integral (lambda (x y) (< y (* x x))) 0 1 0 1)
            (/ 1.0 3.0)
            0.03)))

(define (make-resettable-rand rand-init rand-update)
  (let ((cur-rand (make-rand rand-init rand-update)))
    (lambda (sym)
      (cond
       ((eq? sym 'generate)
        (cur-rand))
       ((eq? sym 'reset)
        (lambda (new-init)
          (set! cur-rand (make-rand new-init rand-update))))
       (else
        (error "-- MAKE-RESETTABLE-RAND: unrecognized symbol" sym ))))))

(module+ test
  (begin-example "3.6, resettable-rand")
  (test-case
   "resettable rand"
   (define rer1 (make-resettable-rand 1 lcg-easy-rand))
   (define rer2 (make-resettable-rand 1 lcg-easy-rand))
   (check-equal? (rer1 'generate) 69070)
   (check-equal? (rer2 'generate) 69070)
   (check-equal? (rer1 'generate) 475628535)
   ((rer1 'reset) 1)
   ;; check it only resets rer1
   (check-equal? (rer1 'generate) 69070)
   (check-equal? (rer2 'generate) 475628535)))


(define (make-joint password-protected-account
                    password
                    new-password)
  ((password-protected-account password 'add-password)
   new-password))

(module+ test
  (begin-example "3.7, making joint accounts")
  
  (test-case
   "make joint account"
   
   (let* ((pete-acc (make-protected-account 100 'open-sesame))
          (paul-acc (make-joint pete-acc 'open-sesame 'rosebud)))

     ;; Check that the balance is shared correctly
     (check-equal?
      ((pete-acc 'open-sesame 'withdraw) 30) 70)
     (check-equal?
      ((paul-acc 'rosebud 'withdraw) 30) 40)
     (check-equal?
      ((pete-acc 'open-sesame 'withdraw) 30) 10)

     ;; can't use Paul's password with Pete's access
     ;; and vice versa
     (check-exn
      (lambda (exn) #t)
      (lambda ()
        ((pete-acc 'rosebud 'withdraw) 1)))
     (check-exn
      (lambda (exn) #t)
      (lambda ()
        ((paul-acc 'open-sesame 'withdraw) 1))))))

(define f
  ;; Since we only need one f, it's shorter to create just one frame
  ;; rather than a function that creates frames when called.
  (let ((called #f))
    (lambda (x)
      (if called
          0
          (begin
            (set! called #t)
            x)))))

(module+ test
  (begin-example "3.8, calling order")
  (test-case
   "Call-dependent f"
   (check-equal? (f 1) 1)
   (check-equal? (f 0) 0)))
  
