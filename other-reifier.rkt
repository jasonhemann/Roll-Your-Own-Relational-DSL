#lang racket
(require "microKanren.rkt")
(provide (all-defined-out))

(define (subterm? u v s)
  (let ((u (walk* u s))
        (v (walk* v s)))
    (letrec
      ((subterm?
        (lambda (v)
          (cond
            ((equal? u v) #t)
            ((pair? v) (or (subterm? (car v)) (subterm? (cdr v))))
            (else #f)))))
      (and (not (equal? u v)) (subterm? v)))))
(define (my-new-reify s/c)
  (let ((s (car s/c)))
    (let ((s (sort s (lambda (u v) (subterm? u v s)) #:key car #:cache-keys? #t)))
      `(,s . ,(cdr s/c)))))