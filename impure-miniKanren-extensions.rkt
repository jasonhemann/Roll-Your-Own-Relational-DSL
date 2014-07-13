#lang racket
(require "miniKanren.rkt")
(require "impure-microKanren-extensions.rkt")
(require "miniKanren.rkt")
(provide (all-defined-out))
(define-syntax project
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/project x0 
       (lambda (x0) (project (x ...) g0 g ...))))))
(define-syntax ifte*
  (syntax-rules ()
    ((_ (g0 g ...)) (conj+ g0 g ...))
    ((_ (g0 g1 g ...) (h0 h ...) ...)
     (ifte g0 (conj+ g1 g ...) (ifte* (h0 h ...) ...)))))
(define-syntax conda
  (syntax-rules ()
    ((_ (g0 g ...) (h0 h ...) ...)
     (inverse-eta-delay
      (ifte* (g0 g ... succeed) (h0 h ... succeed) ... (fail))))))
(define-syntax condu
  (syntax-rules ()
    ((_ (g0 g ...) (h0 h ...) ...)
     (conda ((once g0) g ...) ((once h0) h ...) ...))))
