#lang racket
(require "microKanren.rkt")
(require "impure-microKanren-extensions.rkt")
(provide (all-defined-out))
(define-syntax inverse-eta-delay
  (syntax-rules ()
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))
(define-syntax conj+
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (conj g0 (conj+ g ...)))))
(define-syntax disj+
  (syntax-rules ()
    ((_ g) g)
    ((_ g0 g ...) (disj g0 (disj+ g ...)))))
(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh (lambda (x0) (fresh (x ...) g0 g ...))))))
(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) (g0* g* ...) ...)
     (inverse-eta-delay
      (disj+ (conj+ g0 g ...) (conj+ g0* g* ...) ...)))))
(define (reify-var0 s/c)
  (let ((v (walk* (var 0) (car s/c))))
    (walk* v (reify-s v '()))))
(define (reify-s v s)
  (let ((v (walk v s)))
    (cond
      ((var? v)
       (let ((name (reify-name (length s))))
         (cons (cons v name) s)))
      ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
      (else s))))
(define (reify-name n)
  (string->symbol
    (string-append "_." (number->string n))))
(define (mK-reify ls) (map reify-var0 ls))
(define (pull $) (if (procedure? $) (pull ($)) $))
(define (take* $)
  (let (($ (pull $)))
    (if (null? $) '() (cons (car $) (take* (cdr $))))))
(define (take n)
  (lambda ($)
    (cond
      ((zero? n) '())
      (else
       (let (($ (pull $)))
         (cond
           ((null? $) '())
           (else
            (cons (car $)
             ((take (- n 1)) (cdr $))))))))))
(define-syntax query
  (syntax-rules ()
    ((_ takef g) (mK-reify (takef (call/empty-state g))))))
(define-syntax run*
  (syntax-rules ()
    ((_ (q) g0 g ...) 
     (query take* (fresh (q) g0 g ...)))))
(define-syntax run
  (syntax-rules ()
    ((_ n (q) g0 g ...)
     (query (take n) (fresh (q) g0 g ...)))))
