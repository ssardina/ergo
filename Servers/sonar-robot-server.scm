(include "generic-action-server.scm")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  An example simulated world of 1d robot at a distance from a wall. 
;;;   Two actions: sonar-request! and (move! x)
;;;     sonar-request! returns (sonar-response! x)
;;;   Both actions are noisy according to a Gaussian distribution

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

;; the actual (but unknown) distance to the wall in the simulation.
(define dist 7.26)         

(define (initialize) (eprintf "The actual distance to the wall is ~a\n" dist))
(define (finalize) (eprintf "The final distance to the wall is ~a\n" dist))

(define (service iport oport)
  (let loop ((act (read iport)))
    (unless (eof-object? act)
      (eprintf "  Action received: ~a\n" act)
      (sleep .5)                                        ; action burns .5 secs
      (cond
       ((eq? act 'sonar-request!)                       ; request for sensing
        (let ((sense (GAUSSIAN-GEN dist (max .01 (/ dist 100)))))
          (eprintf "  Generating sonar response of ~a\n" sense)
          (displayln (list 'sonar-response! sense) oport)))
       ((and (list? act) (eq? (car act) 'move!))        ; request for a move
        (let ((x (GAUSSIAN-GEN (cadr act) (/ (abs (cadr act)) 10))))
          (set! dist (max 0 (- dist x)))
          (eprintf "  New distance to the wall is actually ~a\n" dist)))
       (else (eprintf "  Ignoring unrecognized action\n")))
      (loop (read iport)))))
