;;;  This is an adaptation of the elevator that appears in the IJCAI-97
;;;  paper on ConGolog.  The code is in reactive-elevator-run.scm.

(include "reactive-elevator-run.scm")

;;;  - All actions are printed here (the default)
;;;  - Exogenous requests for floors are received on port 8235 as numbers.
;;;  - Other exogenous events (smoke! reset! heat! cold!) come on port 8234.

(define-interface 'in
  (let ((ports (open-tcp-server 8235)))
    (displayln "Ready to receive floor requests." (cadr ports))
    (lambda () 
      (display "Floor number: " (cadr ports)) 
      `(turnon! ,(read (car ports))))))

(define-interface 'in
  (let ((ports (open-tcp-server 8234)))
    (displayln "Ready to receive exogenous events" (cadr ports))
    (lambda ()
      (display "Exog: " (cadr ports)) 
      (read (car ports)))))

(define-interface 'out write-endogenous)

(define (main) (ergo-do #:mode 'online control))
