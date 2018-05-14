#lang sicp
(#%require (only racket module+ format) rackunit racket/trace)
(#%require "../utils/sicp-utils.rkt" "../utils/srfi-1.rkt" "ex1.21.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ex 1.22
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  elapsed-time)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Digression: Some early notes on how to think about Applicatives /
;; Monads in a lisp context and what they might look like applied to
;; timing.  The machinery for timing is thus more elaborate than it
;; needs to be.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set up timing object functions
(#%provide make-timing)
(define (make-timing elapsed base-value) (list (cons 'value base-value) (cons 'elapsed-time elapsed)))
(define (get-field sym) (lambda (alist) (cdr (assoc sym alist))))
(#%provide timing-elapsed)
(define timing-elapsed (get-field 'elapsed-time))
(#%provide timing-base-value)
(define timing-base-value (get-field 'value))

;;
;; flip:: (a->b->c)->(b->a->c)
;; lift:: a->M[a]
;; bind:: (a->M[b])->M[a]->M[b]
;;
;; -- defining map from bind and lift
;; check types
;; (a->b)->a->M[b]        ## 1a  (lift with b for a)
;; (a->b)->M[a]->M[b]     ## map (1a, bind)
;;
;; realize this via
;; map:: (a->b)->M[a]->M[b]
;; map f = bind (lift . f)
;;
;; -- defining apply from bind, lift and map
;; check type constructibility
;; M[a->b]->M[a]->M[b]  ## (map, bind with a->b for a, b for b, ignoring the M[a])
;;
;; As far type constructibility, we can move arguments freely back and
;; forward via flip, etc.  So above we can largely ignore the M[a] in
;; the middle of map, and use bind on the matching surrounding part
;; "(a->b)->...M[b]"
;;
;; To construct apply we need to take care of the order of arguments
;; We start with map, moving M[a] off to the left, using bind, and
;; then moving back M[a].
;;
;; map :: (a->b)->M[a]->M[b]
;;
;; (flip map):: M[a]->(a->b)->M[b]
;;
;; Ma::M[a] => (flip map) Ma :: (a->b)->M[b]
;; Ma::M[a] => bind ((flip map) Ma) :: M[a->b]->M[b]
;; apply1:: M[a]->M[a->b]->M[b]
;; apply1 Ma = bind ((flip map) Ma)
;;
;; apply :: M[a->b]->M[a]->M[b]
;; apply = flip apply1

;; Back to SICP, using the above for some timing machinery 
;;
;; repeated use of apply as above can handle multiple arguments.
;; M[a->b->c]->M[a]->M[b->c]
;; M[a->b->c]->M[a]->M[b]->M[c]
;;
;; For Lisp it's convenient to write this version rather than curry
;; everything.
;;
;; In truth this doesn't obey all the applicative laws (homomorphism
;; at least) but it's still interesting to see how it works out.
(define (timed-lift x)
  (make-timing 0.0 x))

(define (timed-application f . args)
  (let* ((base-f (timing-base-value f))
         (base-args (map timing-base-value args))
         (start-time (runtime))
         (result (apply base-f base-args))
         (elapsed-time (- (runtime) start-time)))
    (make-timing
     (+ (timing-elapsed f)
        (sum (map timing-elapsed args))
        elapsed-time)
     result)))

(define (timed-apply f args)
  (apply timed-application (timed-lift f) (map timed-lift args)))

(define (timed-eval f . args)
  (timed-apply f args))

(define (timed f)
  (lambda args
    (timed-apply f args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; back to SICP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(#%provide take-n-primes-timed)
(define (take-n-primes-timed to-take next prime? from)
  (define (iter to-test needed timings)
    (if (= needed 0)
        (reverse timings)
        (let ((timing ((timed prime?) to-test)))
          (if (timing-base-value timing)
              (iter (next to-test)
                    (dec needed)
                    (cons (make-timing (timing-elapsed timing) to-test)
                          timings))
              (iter (next to-test) needed timings)))))
  (iter from to-take '()))

(#%provide time-to-test-primes)
(define (time-to-test-primes to-take prime? next)
  (lambda (from)
    ;; Returns average time to use prime? to test the first to-take
    ;; primes found by checking (from, from+2, from+4, ...)
    (mean (map timing-elapsed (take-n-primes-timed to-take next prime? from)))))

(define (inc-*10-dec x)
  (inc (* 10 (dec x))))

(module+ main
  (begin-example "1.22")

  (displn "First 3 primes >= 10^n+1 for n in (3,4,5,6)")
  (for-each
   (lambda (power)
     (displn
      (map timing-base-value
           (take-n-primes-timed 3 (add 2) prime? (inc (expt 10 power))))))
   (iota 4 3))
  (newline)
  
  (displn "Sample timings from time-to-test-primes")
  (for-each
   (lambda (power)
     (displn
      (list power
            ((time-to-test-primes 10 prime? (add 2)) (inc (expt 10 power))))))
   (iota 10 3))
  (newline)

  (displn "Estimating order of time-to-test-primes")
  (displn
   (estimate-order-ex (time-to-test-primes 10 prime? (add 2)) 1001 (expt 10 14) 1e6 inc-*10-dec))
  (displn "c.f. expected order of 0.5"))
