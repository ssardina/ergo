;;;  This is an offline program for the robotic version of the Towers of Hanoi.

(include "hanoi.scm")

;; pick from stack x and then put to stack y.  Stop when Peg B is empty
(define (picker x y z)
  (:begin (:act (pick! x))
      (:if (known? (eq? hand 'empty))         ; the pick action failed
           (:unless (eq? x 'B) (picker y z x))
           (putter y z x))))

;; put to stack x and then pick from stack y
(define (putter x y z)
  (:begin (:act (put! x))
      (:if (known? (eq? hand 'empty))         ; the put action was successful
           (picker y z x) 
           (putter y z x))))

(define (main) (ergo-gendo (picker 'A 'B 'C)))
