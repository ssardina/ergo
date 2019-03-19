#lang scheme

;;;  This file contains four functions for generating random numbers
;;;  and four functions for testing the likelihood of random numbers

(provide UNIFORM-GEN DISCRETE-GEN BINARY-GEN GAUSSIAN-GEN
         UNIFORM DISCRETE BINARY GAUSSIAN)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Generating random numbers

;; linear transform for generating uniform numbers
(define (UNIFORM-GEN low high) (+ low (* (- high low) (random))))

;; generating discrete values vi from v1 p1 ... vn pn, where sum of pi=1
(define (DISCRETE-GEN . args)
  (let ((r (random)))
    (let loop ((args args) (sum 0))
      (let ((next (+ sum (cadr args))))
        (if (<= r next) (car args) (loop (cddr args) next))))))

;; generating binary values with given prob for #t 
(define (BINARY-GEN p) (DISCRETE-GEN #t p #f (- 1 p)))

;; box muller transform for generating Gaussian numbers
(define cached-gaussian #f)
(define (GAUSSIAN-GEN mu sigma)
  (if cached-gaussian 
      (let ((x2 cached-gaussian)) 
        (set! cached-gaussian #f)
        (+ (* x2 sigma) mu))
      (let loop ()
        (let* ((z1 (- (* 2. (random)) 1.)) (z2 (- (* 2. (random)) 1.))
               (rsq (+ (* z1 z1) (* z2 z2))))
          (if (>= rsq 1.) (loop)
              (let* ((d (sqrt (/ (* -2. (log rsq)) rsq)))
                     (x1 (* z1 d)) (x2 (* z2 d)))
                (set! cached-gaussian x2)  ; save the x2 value
                (+ (* x1 sigma) mu)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Testing random numbers

;; uniform distribution of x with lo and high
(define (UNIFORM x low high)
  (if (and (> x low) (< x high)) (/ 1.0 (- high low)) 0.0))

;; discrete distribution with values vi from v1 p1 ... vn pn, where sum of pi=1
(define (DISCRETE x . args)
  (let loop ((args args))
    (if (null? args) 0.0
        (if (eq? (car args) x) (cadr args) (loop (cddr args))))))

;; binary distribution of x
(define (BINARY x p) (DISCRETE (not (not x)) #t p #f (- 1 p)))

;; normal distribution of x with mean mu and std deviation sigma
(define (GAUSSIAN x mu sigma)
  (let ((var2 (* 2.0 sigma sigma)))
    (/ (exp (- (/ (* (- x mu) (- x mu)) var2))) (sqrt (* var2 pi)))))

