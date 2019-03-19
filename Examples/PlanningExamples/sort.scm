;;; A two-armed robot must sort a stack of bricks by size.  Initially, all the
;;; bricks are located on stackA; in the final state they must all be on
;;; stackB in ascending order.  The robot can pick up a brick from either
;;; stack using either arm and put a brick down onto either stack with either
;;; arm.  When the robot is holding a brick in each hand, it can sense which
;;; one is the bigger of the two.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  the action theory

;; auxilliary functions
(define (pick-result st) (if (null? st) 'fail 'ok))
(define (pick-obtain st) (if (null? st) 'nothing (car st)))
(define (pick-remaining st) (if (null? st) st (cdr st)))

(define (rand-list n)
  (if (= n 0) (list '()) 
      (for/append ((x (rand-list (- n 1)))) (insert n x))))

(define (insert n x)
  (cons (cons n x) 
        (if (null? x) '()
            (for/list ((e (insert n (cdr x)))) (cons (car x) e)))))

;; state made up of 4 fluents
(define-states ((x (for/append ((n 6)) (rand-list n))))
  stackA  x         stackB  '()
  handL   'nothing  handR   'nothing)

(define-action pickLA 
  #:prereq  (eq? handL 'nothing)
  #:sensing (pick-result stackA)
  stackA    (pick-remaining stackA)
  handL     (pick-obtain stackA))

(define-action pickLB
  #:prereq  (eq? handL 'nothing)
  #:sensing (pick-result stackB)
  stackB    (pick-remaining stackB)
  handL     (pick-obtain stackB))

(define-action pickRA 
  #:prereq  (eq? handR 'nothing)
  #:sensing (pick-result stackA)
  stackA    (pick-remaining stackA)
  handR     (pick-obtain stackA))

(define-action pickRB 
  #:prereq  (eq? handR 'nothing)
  #:sensing (pick-result stackB)
  stackB    (pick-remaining stackB)
  handR     (pick-obtain stackB))

(define-action putLA
  #:prereq  (not (eq? handL 'nothing))
  stackA    (cons handL stackA)
  handL     'nothing)

(define-action putLB
  #:prereq  (not (eq? handL 'nothing))
  stackB    (cons handL stackB)
  handL     'nothing)

(define-action putRA
  #:prereq  (not (eq? handR 'nothing))
  stackA    (cons handR stackA)
  handR     'nothing)

(define-action putRB
  #:prereq  (not (eq? handR 'nothing))
  stackB    (cons handR stackB)
  handR     'nothing)

(define-action senseBig
  #:prereq  (and (not (eq? handL 'nothing)) (not (eq? handR 'nothing)))
  #:sensing (if (> handL handR) 'left 'right)) 

;; the goal to be achieved
(define (goal?)
  (and (eq? handL 'nothing)
       (eq? handR 'nothing)
       (null? stackA)
       (sorted? stackB)))

;; sorted iff in ascending order
(define (sorted? x)
  (or (null? x)
      (null? (cdr x))
      (and (<= (car x) (cadr x)) (sorted? (cdr x)))))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  the main program

(define (main)
  (ergo-genplan goal?
      (list pickLA pickLB pickRA pickRB putLA putLB putRA putRB senseBig)
      #:steps 160  #:loop? #f  #:states 6))
