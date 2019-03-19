;;;  This is the ERGO code for a robot car doing a JoyRide
;;;  It communicates with a running Unity 3d engine.

(include "u3d-bridge.scm")                   ; bridge to the Unity 3d Engine

(define-fluents 
  distance    0                              ; how far to an object (0 = clear)
  turning?    #f                             ; am I turning?
  advancing?  #f)                            ; am I going forward? 

;; four normal actions
(define-action right-turn! turning? #t)
(define-action straight! turning? #f)
(define-action go! advancing? #t)
(define-action stop! advancing? #f)

;; one exogenous action, reporting on objects ahead 
(define-action (object-detect! d) distance d)

;; the car controller
(define (control)
  (:monitor
     (:when (and (> distance 0) (< distance 20) advancing?) 
         (:act stop!) (:unless turning? (:act right-turn!)))
     (:when (and (> distance 0) (< distance 60) (not turning?))
         (:act right-turn!))
     (:when (= distance 0)
         (:when (not advancing?) (:act go!))
         (:when turning? (:act straight!)))
     (:while #t (:wait))))

(define (main . args)
   (u3d-start-comm 8123 (not (null? args)))  ; set up online interfaces
   (ergo-do #:mode 'online (control)))       ; run the ERGO program     

