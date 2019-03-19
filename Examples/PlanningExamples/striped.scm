;;;  A robot world just like the Two Towers problem but with a harder goal.

;; all lists of length n made up of only red and blue elements
(define (all-blocks n)
  (if (= n 0) '(())
      (let ((x (all-blocks (- n 1))))
        (append (for/list ((e x)) (cons 'blue e))
                (for/list ((e x)) (cons 'red e))))))

;; all lists of length n with an even number of blue and red elements
(define (even-blocks n)
  (for/only ((x (all-blocks n)))
    (= (for/sum ((b x)) (if (eq? b 'blue) 1 0)) (/ n 2))))

;; stack A: up to 6 blocks with equal number of red and blue ones
(define stackA-values (for/append ((n 4)) (even-blocks (* n 2))))

(include "red-blue-bat.scm")    ; the BAT from the Two Towers problem

;; the goal to be achieved: a striped tower on stack C
(define (goal?)
  (and (eq? hand 'empty) (null? (stack 'A)) (null? (stack 'B))
       (striped? (stack 'C)) ))

;; the recursive definition of a striped tower with blue at the top
(define (striped? x)
  (or (null? x) 
      (and (eq? (car x) 'blue) (eq? (cadr x) 'red) (striped? (cddr x)))))
          
;; find a plan that works for all states with up to 6 blocks
(define (main)
  (ergo-genplan #:states 10  #:loop? #t  #:loose? #f
          goal? (append (map pick! '(A B)) (map put! '(B C)))))
      

