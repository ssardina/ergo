;;; This is a program for a delivery agent on a 2d grid using :monitor
;;; The objects needing delivery and their goal destinations are fixed.

(define objects '(a b c d))
(define (goal x) (case x ((a) '(3 4)) ((b) '(4 2)) ((c) '(0 3)) ((d) '(1 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  The basic action theory

(define-fluents
    rob-loc  '(0 0)                                               ; rob loc
    loc-table (hasheq 'a '(0 0) 'b '(4 0) 'c '(3 3) 'd '(3 3)))   ; obj locs

;;  abbreviations
(define (obj-loc x) (hash-ref loc-table x))       ; obj loc or #f
(define (carried? x) (not (obj-loc x)))           ; obj is being carried
(define (home? x) (equal? (obj-loc x) (goal x)))  ; obj is at goal?

(define-action (move! dir)           ; dir is (0 1) (0 -1) (1 0) or (-1 0)
    rob-loc (list (+ (car rob-loc) (car dir)) (+ (cadr rob-loc) (cadr dir))))

(define-action (pickup! x)
    #:prereq (equal? rob-loc (obj-loc x))      ; prereq: rob and x co-located
    loc-table (hash-set* loc-table x #f))      ; the #f says x is being carried

(define-action (putdown! x)
    #:prereq (carried? x)                      ; prereq: x is being carried
    loc-table (hash-set* loc-table x rob-loc)) ; x goes on the grid at rob-loc

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  The main program for the robot

(define (go-to d)                              ; go to loc d by a seq of moves
  (:begin (:while (< (car rob-loc) (car d)) (:act (move! '(1 0))))
          (:while (> (car rob-loc) (car d)) (:act (move! '(-1 0))))
          (:while (< (cadr rob-loc) (cadr d)) (:act (move! '(0 1))))
          (:while (> (cadr rob-loc) (cadr d)) (:act (move! '(0 -1)))) ))

(define (main) (ergo-do
   (:monitor 
      ; pickup or dropoff an object as required during motion
      (:for-all x objects
           (:when (and (equal? (goal x) rob-loc) (carried? x))
               (:act (putdown! x)))
           (:when (and (equal? (obj-loc x) rob-loc) (not (home? x)))
               (:act (pickup! x)) ))
      ; move to the location of an object or carry it to its goal
      (:until (and-map home? objects)
           (:for-some x (for/only ((x objects)) (not (home? x)))
               (:let ((dest (or (obj-loc x) (goal x))))
                  (go-to dest))))                         )))
