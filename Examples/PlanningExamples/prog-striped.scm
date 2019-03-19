;;;  The striped tower problem as a generalized program

(include "striped.scm")

(define (opp-colour x) (if (eq? x 'red) 'blue 'red))

(define (get-colour col)
  (:begin (:act (pick! 'A))
          (:unless (known? (eq? hand 'empty))
            (:if (known? (not (eq? hand col))) 
              (:begin (:act (put! 'B)) (get-colour col))
              (:begin (:act (put! 'C))
                      (:act (pick! 'B))
                      (:if (known? (eq? hand 'empty))
                         (get-colour (opp-colour col))
                         (:begin (:act (put! 'C)) (get-colour col))))))))

;; find a plan that works for all states with up to 6 blocks
(define (main) (ergo-gendo (get-colour 'red)))
      

