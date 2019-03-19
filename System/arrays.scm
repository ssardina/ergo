#lang racket

;;; This provides a cheap multi-dimensional array facility.  For example
;;;    (define x (build-array (list 3 5) (lambda (x y) 13)))
;;;    (array-ref x (list 0 2))  ==> 13
;;;    (array-set! x (list 0 3) 4) then (array-ref x (list 0 3)) ==> 4

(provide build-array array-ref array-set array-set* make-array array-set!)

;; calculate a vector index given the multi-dimensional array indices
;; << this is not the most efficient way, but # of dims should be small >>
(define (dope-vector dims inds)
  (if (null? (cdr inds)) (car inds)
      (+ (* (car inds) (apply * (cdr dims))) 
         (dope-vector (cdr dims) (cdr inds)))))

;; the internal data structure: a list of dimensions and a single vector
(define-struct :array (dims vec) #:prefab)

;; make an array with given dims, whose initial values are val
(define (make-array dims val)
  (make-:array dims (make-vector (apply * dims) val)))

;; all the indices for given dims as a list
(define (all-inds dims)
  (if (null? dims) '(())
      (let ((z (all-inds (cdr dims))))
        (append-map (lambda (i) (for/list ((u z)) (cons i u)))
                    (for/list ((i (car dims))) i)))))
  
;; make an array with given dims, whose initial values are valfn of indices
(define (build-array dims valfn)
  (make-:array dims
   (apply vector (for/list ((inds (all-inds dims))) (apply valfn inds)))))

;; return array element indexed by inds
(define (array-ref arr inds)
  (vector-ref (:array-vec arr) (dope-vector (:array-dims arr) inds)))

;; change array element indexed by inds
(define (array-set! arr inds val)
  (vector-set! (:array-vec arr) (dope-vector (:array-dims arr) inds) val))

;; make a copy of an array
(define (array-copy old) 
  (make-:array (:array-dims old) (vector-copy (:array-vec old))))

;; copy then set!
(define (array-set arr inds val)
  (let ((new (array-copy arr)))
    (vector-set! (:array-vec new) (dope-vector (:array-dims new) inds) val)
    new))

;; copy then set*!
(define (array-set* arr . pairs)
  (let* ((new (array-copy arr)) (v (:array-vec new)) (d (:array-dims new)))
    (let loop ((p pairs))
      (unless (null? p)
        (vector-set! v (dope-vector d (car p)) (cadr p))
        (loop (cddr p))))
    new))
