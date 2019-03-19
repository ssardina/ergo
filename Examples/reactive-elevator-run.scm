;;;  This is the main program for the elevator that appears in the IJCAI-97
;;;  paper on ConGolog.  The BAT appears in reactive-elevator-bat.scm

(include "reactive-elevator-bat.scm")

;; get to floor n using up and down actions
(define (go_floor n)
   (:until (= floor n) (:if (< floor n) (:act up!) (:act down!))))

;; the main elevator program as a priority driven monitor
(define control
  (:monitor
     (:when (and (< temp -2) fan?) (:act toggle-fan!))      ; handle cold
     (:when (and (> temp 2) (not fan?)) (:act toggle-fan!)) ; handle heat
     (:while alarm? (:act ring!))                  ; stop and ring the bell
     (:until (null? on-buttons)                    ; main serving behaviour
        (:for-some n on-buttons                             ; choose a floor
             (go_floor n) (:act turnoff!)))                 ; serve it
     (go_floor 1)                                  ; default homing behaviour
     ;; (:while #t (:wait))        ; to keep the elevator running when online
))
