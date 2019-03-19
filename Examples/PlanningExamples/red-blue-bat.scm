;;;  A simple robot world involving objects on three stacks, A, B, and C.
;;;  There are two fluents
;;;      hand    the object held in the robot's hand or `empty'
;;;      stacks  the contents of each stack as a list of objects
;;;  There are two actions:
;;;      pick!   attempt to pick up the first object from a stack
;;;      put!    push an object onto a stack

;; use stackA-values defined elsewhere to get the values for stack A
(define-states ((ini stackA-values))
    hand   'empty                          
    stacks (hasheq 'A ini 'B '() 'C '()))

;; abbreviation to get the content of a stack A, B, or C
(define (stack st) (hash-ref stacks st))

;; action to pick up an object and return either its colour or 'fail'
(define-action (pick! st)
   #:prereq   (eq? hand 'empty)
   #:sensing  (if (null? (stack st)) 'fail (car (stack st)))
   hand       (if (null? (stack st)) 'empty (car (stack st)))
   stacks     (if (null? (stack st)) stacks 
                  (hash-set* stacks st (cdr (stack st)))) )

;; action to push what is being held to the top of a stack
(define-action (put! st)
   #:prereq   (not (eq? hand 'empty))
   hand       'empty
   stacks     (hash-set* stacks st (cons hand (stack st)))  )
