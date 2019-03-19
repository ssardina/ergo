;;;  This is a program for the Two Towers problem

;; possible initial values for stack A 
(define stackA-values '(() (red) (blue) (red red blue red)))

;; the BAT 
(include "red-blue-bat.scm")

;; make a tower of reds on stack B and a tower of blues on stack C
(define (goal?)
   (and (eq? hand 'empty) (null? (stack 'A))
        (for/and ((o (stack 'B))) (eq? o 'red))
        (for/and ((o (stack 'C))) (eq? o 'blue))))

(define (main)
  (ergo-genplan goal? (append (map pick! '(A B C)) (map put! '(A B C)))))
