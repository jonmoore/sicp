#lang racket

(require racket/generator)

(define (stdin-lines)
  (in-lines (current-input-port)))

(define (display-line-with-newline line)
  (display line)
  (newline))

(define (display-strings lines)
  (sequence-for-each
   display-line-with-newline
   lines))

(define (matching-lines pattern lines)
   (sequence-filter
    (lambda (line) (regexp-match pattern line))
    lines))  

(define (grep pattern)
  (display-strings
   (matching-lines pattern (stdin-lines))))

(define (matching-strings pattern lines)
  (in-generator
   (for* ([line lines]
          [matching-string (regexp-match* pattern line)])
     (yield matching-string))))

(define (matches pattern)
  (display-strings
   (matching-strings pattern (stdin-lines))))

(module+ main
  (let* ((args (vector->list (current-command-line-arguments)))
         (command (car args))
         (pattern (pregexp (cadr args))))
    (cond
     [(equal? command "grep")
      (grep pattern)]
     [(equal? command "matches")
      (matches pattern)]
     [else
      (raise-user-error
       "Command must be grep or matches.  The command passed was:"
       command)])))
