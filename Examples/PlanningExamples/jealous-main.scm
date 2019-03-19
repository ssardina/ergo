;;; The Jealous husband problem:
;;;   Three couples on the left side of the river
;;;   A boat that can hold 1 or 2 people.
;;;   The goal: get everyone to the right side of the river. 
;;;   Constraint: no woman can be with a man unless her husband is present

(include "jealous-bat.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Goal state, bad state, and the list of all allowed actions

;; a goal state is one where all the people are on the right side
(define (goal?) (for/and ((p people)) (eq? (loc p) 'right)))

;; a bad state is one that violates the above jealousy constraint
(define (bad?)
  (for/or ((w women)) (and (not (eq? (loc w) (loc (husband w))))
                           (for/or ((m men)) (eq? (loc w) (loc m))))))

;; all single-person crossings
(define all-single-crossings (map cross-single! people))

(define (can-boat? p1 p2)                     ; p1 and p2 can boat together?
  (and (or (memq p1 men) (memq p2 women) (eq? p2 (husband p1)))
       (or (memq p2 men) (memq p1 women) (eq? p1 (husband p2)))))

;; all double-person crossings where the first person occurs before the
;; second in the list of people and where the two can boat together
(define all-double-crossings
  (for/append ((p1 people))
    (for/list ((p2 (for/only ((p2 (cdr (memq p1 people)))) (can-boat? p1 p2))))
      (cross-double! p1 p2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   The main program

(define (main)
  (ergo-simplan goal? (append all-single-crossings all-double-crossings) 
           #:loop? #t
           #:prune bad?))
