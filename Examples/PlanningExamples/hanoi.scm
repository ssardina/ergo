;;;  This is a program for the robotic version of the Towers of Hanoi.

(define-states ((ini '((1) (1 2) (1 2 3) (1 2 3 4)))) 
   hand   'empty                             ; start holding nothing
   stacks (hasheq 'A ini 'B '() 'C '()))     ; all stacks empty

;; abbreviation to get the content of a stack A, B, or C
(define (stack st) (hash-ref stacks st))

;; the contents of hand cannot be placed on stack st
(define (cannot-put? st)
  (and (not (null? (stack st))) (> hand (car (stack st)))))

;; action to pick up an object and return either 'ok' or 'fail'
(define-action (pick! st)
   #:prereq   (eq? hand 'empty)
   #:sensing  (if (null? (stack st)) 'fail 'ok)
   hand       (if (null? (stack st)) 'empty (car (stack st)))
   stacks     (if (null? (stack st)) stacks 
                  (hash-set stacks st (cdr (stack st)))) )

;; action to put what is being held on a stack and return 'ok' or 'fail'
(define-action (put! st)
   #:prereq   (not (eq? hand 'empty))
   #:sensing  (if (cannot-put? st) 'fail 'ok)
   hand       (if (cannot-put? st) hand 'empty)
   stacks     (if (cannot-put? st) stacks
                  (hash-set stacks st (cons hand (stack st))) ))

;; get all the disks onto stack C
(define (goal?) (and (eq? hand 'empty) (null? (stack 'A)) (null? (stack 'B))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  The main program

(define (main) 
  (ergo-genplan goal? (append (map pick! '(A B C)) (map put! '(A B C)))
                #:steps 60))
