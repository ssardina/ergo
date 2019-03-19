;;;  This is an adaptation of the elevator that appears in the IJCAI-97
;;;  paper on ConGolog.  It uses exogenous actions for temperature, smoke,
;;;  and call buttons.  The actions and fluents are described below.

(define-fluents
   floor  7                             ; the floor the elevator is on
   temp   0                             ; the temperature inside the elevator
   fan?   #f                            ; is the fan on?
   alarm? #f                            ; is the smoke alarm on?
   on-buttons '(3 5) )                  ; the floors that are being called

;; the normal actions
(define-action up!                      ; go up one floor
   #:prereq (< floor 10)  floor (+ floor 1))
(define-action down!                    ; go down one floor
   #:prereq (> floor 1)   floor (- floor 1))
(define-action toggle-fan!              ; toggle the fan
   fan? (not fan?))
(define-action turnoff!                 ; turn off button for current floor
   on-buttons (remove floor on-buttons))
(define-action ring!)                   ; ring the alarm bell

;; the exogenous actions
(define-action (turnon! n)              ; call button n is turned on
   on-buttons (if (memq n on-buttons) on-buttons (cons n on-buttons)))
(define-action heat!  temp (+ temp 1))  ; the temperature rises
(define-action cold!  temp (- temp 1))  ; the temperature falls
(define-action smoke! alarm? #t)        ; smoke activates alarm
(define-action reset! alarm? #f)        ; alarm is deactivated
