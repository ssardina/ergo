;;;  This program provides agent behaviour in the Squirrel World (SW)
;;;  The squirrel finds a wall ahead, and then runs back and forth.

(include "sw-bridge.scm")              ; define the interfaces to SW
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Basic action theory

(define-fluents 
   steps  0                            ; the number of steps taken
   seen   'nothing)                    ; the last thing seen by "look"

;;  squirrel actions
(define-action left)                   ; turn left
(define-action look)                   ; look ahead
(define-action forward                 ; go forward
   steps  (+ steps 1))

;;  exogenous action
(define-action (set-view! x)           ; report from last look action
   seen   x)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Control program

(define (count)                        ; go forward until a wall is seen
   (:until (eq? seen 'wall) (:act forward) (:act look) (:wait)))

(define (run n)                        ; forever do 2 lefts and n forwards
   (:while #t (:act left) (:act left) (:for-all i n (:act forward))))

(define (main)                         ; overall: count then run
   (ergo-do #:mode 'online (:begin (count) (:let ((n steps)) (run n)))))
