;;; This program uses the EV3 Robot Manager 1 to perform the actions below.
;;; Once the robot manager is running, this ERGO program can be run by
;;;     racket -l ergo -f test-manager1.scm -m <IPaddress>
;;; where <IPaddress> is the address of the EV3 machine.

(include "tag-bridge.scm")

(define-fluents light 100)

;; endogenous
(define-action (run_motor! r x))
(define-action (req_sensor! r))
;; exogenous
(define-action (reply_sensor! r z) light z)

(define (main . args)
  (and (null? args) (error "Must supply an IP address for EV3"))
  (tag-tcp-setup 'my-EV3 8123 (car args))       ; port 8123 assumed
  (ergo-do #:mode 'online
     (:begin (:act (run_motor! 'my-EV3 2000))   ; run the motor for 2000 ms
             (:act (req_sensor! 'my-EV3))       ; ask for a sensor reading
             (:wait)                            ; wait for value returned
             (:>> (printf "The light had value ~a\n" light)))))
