(include "sw-bridge.scm")
(include "systematic-bat.scm")
(include "systematic-procs.scm")

(define-fluent path '())
(define-action push-path! path (cons direction path))
(define-action pop-path! path (cdr path))

(define (revdir dir)             ; the 180 direction from dir
  (cadr (assq (cadr (assq dir dirL)) dirL)))

(define (do-at-nest prog)        ; go to nest, do prog, then return
  (:begin (:for-all d path (go-dir (revdir d)))
          prog
          (:for-all d (reverse path) (go-dir d))))

(define (take-step r)            ; try a step in a random dir according to r
  (define (try-in-order dirs)
    (:begin (face-dir (car dirs)) (check look)
            (:if (eq? seen 'nothing)
                 (:begin (:act push-path!) (:act forward) (check smell))
                 (try-in-order (cdr dirs)))))
  (try-in-order
    (if (< r .48) '(north east west south)
        (if (< r .96) '(east north south west) '(south west north east)))))

(define (path-restart)           ; go back to nest and start a new path
  (:until (null? path) (go-dir (revdir (car path))) (:act pop-path!)))

(define (main)
   (ergo-do #:mode 'online       ; random search of grid
     (:begin (go-dir 'north) (go-dir 'east)        ; start from (1,1) location
        (:monitor (low-energy) (acorns-present)
          (:while #t                
             (path-restart)                        ; start search over
             (:for-all i 35 (take-step (random)))  ; take 35 random steps
             (check feel) (status-report))))))

