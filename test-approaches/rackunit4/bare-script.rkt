;; Example for testing using rackunit
;;
;; This file is a script whose definitions we want to test
;; externally.

;; gotcha - putting "#lang sicp" or "#lang racket" or "lang
;; racket/load" at the top of the script results in racket not being
;; able to find the definitions made in that file if we (load ...) it.
;;
;; So instead we leave out the #lang and now the tests run.  Not
;; ideal.

;; running this
;;
;; 1. racket command line
;;   racket -l sicp -f sicp-script.rkt
;; 2. test program
;;   raco test sicp-test-script.rkt
;;
;; 3. emacs.  We want to run as sicp if we can.  See
;; https://stackoverflow.com/a/21025189
;;
;; Entering a file's context (like C-c C-a)
;; ,enter "<filepath>/file.rkt"
;;
;; "a language obtains a module path by trying two locations: first, it
;; looks for a reader submodule of the main module for language. If
;; this is not a valid module path, then language is suffixed with
;; /lang/reader. c.f. sicp2/lang/reader.rkt
;;
;; A consequence of the way that a #lang language is turned into a
;; module path is that the language must be installed in a collection,
;; similar to the way that "racket" or "slideshow" are collections
;; that are distributed with Racket."


(define (double x)
  (* x 2))

(define (triple x)
  (* x 3))
