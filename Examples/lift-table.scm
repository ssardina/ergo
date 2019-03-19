;;;  Two robots must lift two ends of a table in increments up to a height
;;;  while ensuring that the table remains level (to a tolerance)

;;;  Two fluents:  position of table ends, and holding status of each robot
;;;  Two actions:  grab an end of a table, and move vertically

(define robots '(r1 r2))                ; the robots
(define ends '(e1 e2))                  ; the table ends

(define goal-height 6)                  ; the desired height
(define amount 1)                       ; the increment for lifting
(define tolerance 1)                    ; the tolerance 

(define-fluents
   pos-table  (hasheq 'e1 0 'e2 0)      ; vertical pos of table end
   held-table (hasheq 'r1 #f 'r2 #f))   ; what robot is holding (#f = nothing)
                 
;; some useful abbreviations
(define (pos e) (hash-ref pos-table e))
(define (held r) (hash-ref held-table r))
(define (table-is-up?)                  ; both ends higher than goal-height?
  (for/and ((e ends)) (>= (pos e) goal-height)))
(define (safe-to-lift? r z)             ; ok for robot to lift its end?
  (let ((e (held r)))
    (let ((e* (for/or ((d ends)) (and (not (eq? e d)) d))))
      (<= (pos e) (+ (pos e*) tolerance (- z))))))

;; action of robot r grabbing table end e
(define-action (grab! r e) 
   #:prereq    (and (for/and ((r* robots)) (not (eq? e (held r*))))
                    (not (held r)))
   held-table  (hash-set* held-table r e))

;; action of robot r moving his end of the table up by z units
(define-action (lift! r z)
   #:prereq    (and (held r) (safe-to-lift? r z))
   pos-table   (let ((e (held r))) (hash-set pos-table e (+ (pos e) z))))
               

;; the main lifting program for robot r
(define (lifter r)
   (:begin (:for-some e ends (:act (grab! r e)))              ; grab an end
           (:until (table-is-up?) (:act (lift! r amount)) ))) ; lift repeatedly

;; the main program: run both robots concurrently
(define (main) (ergo-do (:conc (lifter 'r1) (lifter 'r2))))
