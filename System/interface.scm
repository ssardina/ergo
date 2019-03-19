#lang racket
(require (lib "defmacro.ss"))
(provide ergo-read  define-interface
         add-ifc-exog add-ifc-endo ifc-exog-acts ifc-endo-acts
         read-exogenous write-endogenous)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reading and writing of endogenous and exogenous actions

;; read with a prompt
(define (ergo-read prompt) (display prompt) (flush-output) (read))

;;  This is a vanilla procedure that can be used for reading exog actions
;;  Note: there is no error checking!
(define (read-exogenous) 
  (let ((act (ergo-read #"Act: ")))
    (printf "<<< Exogenous act: ~a\n" act) 
    act))

;;  This is a vanilla procedure that can be used for writing endogenous actions
;;  Note: each action here causes a delay of 1.5 seconds
(define (write-endogenous act) 
  (printf ">>> Executing act ~a\n" act) (sleep 1.5))

;;  The global lists of exogenous and endogenous interfaces
(define ifc-exog-acts '())
(define ifc-endo-acts '())

;;  To add one of the default interfaces
(define (add-ifc-exog new) (set! ifc-exog-acts (cons new ifc-exog-acts)))
(define (add-ifc-endo new) (set! ifc-endo-acts (cons new ifc-endo-acts)))

;;  Main way to define an interface in a program
(define-macro (define-interface dir expr)
  (let ((lam `(lambda () ,expr)))
    `(case ,dir
       ((out) (add-ifc-endo ,lam))
       ((in) (add-ifc-exog ,lam))
       (else (error "~a is an incorrect argument to define-interface" dir)))))
