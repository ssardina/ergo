;;; The classical fox, hen, and grain problem.
;;;    Fluent locs: the side of the river that the four objects are on
;;;    Action cross-with!: farmer crosses the river with a passenger or alone 
 
;; Initially all four objects are on the left side
(define-fluents locs (hasheq 'farmer 'left 'fox 'left 'grain 'left 'hen 'left))

;; The farmer crosses with x (when x=farmer, then cross alone) 
(define-action (cross-with! x)
  #:prereq  (same-side? 'farmer x)
  locs      (let ((other (if (eq? (loc 'farmer) 'left) 'right 'left)))
              (hash-set* locs 'farmer other x other)))

;;  Abbreviations
(define objects '(farmer fox hen grain))
(define (loc x) (hash-ref locs x))
(define (same-side? x y) (eq? (loc x) (loc y)))

;; Goal state: all four objects on the right side
(define (goal?) (for/and ((x objects)) (eq? (loc x) 'right)))

;; Unsafe state: the hen is with the grain or fox without the farmer present
(define (unsafe?)
  (and (or (same-side? 'hen 'grain) (same-side? 'fox 'hen))
       (not (same-side? 'farmer 'hen))))

;;  The main program: display a plan to achieve the goal
(define (main)
  (display-execution
    (lambda () (for ((x objects)) (printf "~a at ~a  " x (loc x))))
    (ergo-simplan goal? #:prune unsafe? (map cross-with! objects))))
