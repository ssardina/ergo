;;;  This is program for a robot with incomplete knowledge that starts out
;;;  somewhere between 2 and 12 units away from the wall, and uses a noisy
;;;  sensor and noise effector to move to close to 5 units away from the wall

(define-states ((i 1000000))
  h (UNIFORM-GEN 2.0 12.0))                             ; distance to wall

(define-action (move! x)                                ; advance x units
  h (max 0 (- h (* x (GAUSSIAN-GEN 1.0 .2)))))

(define-action sonar-request!)                          ; sonar endogenous
(define-action (sonar-response! z)                      ; sonar exogenous
  weight (* weight (GAUSSIAN z h .4)))

(define (get-close x)
  (:let loop ()
    (:while (> (sample-variance h) .02)              ; too much uncertainty?
       (:begin (:act sonar-request!) (:wait)))       ; get sonar data
    (:let ((d (- (sample-mean h) x)))                ; get distance to x
       (:when (> (abs d) .05)                        ; |d| is still large?
          (:act (move! d))                           ; move the robot
          (loop)))))                                 ; repeat

;; program interacts with an external robot manager on port 8123
(define (main)
  (let ((ports (open-tcp-client 8123)))
    (define-interface 'in (lambda () (read (car ports))))
    (define-interface 'out (lambda (act) (displayln act (cadr ports)))))
  (ergo-do #:mode 'online (get-close 5)))
