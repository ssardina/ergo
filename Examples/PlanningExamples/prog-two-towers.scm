;;;  This is an offline program for the Two Towers problem

(include "two-towers.scm")

(define (control) 
  (:begin (:act (pick! 'A))
      (:until (known? (eq? hand 'empty))
          (:if (known? (eq? hand 'red)) (:act (put! 'B)) (:act (put! 'C)))
          (:act (pick! 'A)))))

;; find a plan that works for all the initial states
(define (main) (ergo-gendo (control)))
