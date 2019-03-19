;;; The main procedure defines a wall-hugging (mouse-like) squirrel.
;;; It first goes out tentatively, checking for walls, first north, then east.
;;; Once it finds its north and east walls, it runs these paths full speed,
;;; ignoring acorns and other squirrels, until it runs out of energy.

(define tcp-out '())
(define tcp-in '())

(define (tcp-init) 
  (let-values ([(iport oport) (tcp-connect "localhost" 8123)])
     (define name (read iport))
     (printf "Made connection for ~a\nStart wall following\n" name)
     (set! tcp-out oport)
     (set! tcp-in iport)))

(define (do-action act)
  (call-with-exception-handler stop-and-exit
     (lambda ()
       (write act tcp-out) (newline tcp-out) (flush-output tcp-out)       
       (read tcp-in))))

(define (stop-and-exit any)
  (display "Network connection is gone\n")
  (exit 0))

;; run full speed n steps
(define (run n) (when (> n 0) (do-action 'forward) (run (sub1 n))))

;; walk ahead until a wall is sensed, returning the number of steps
(define (pace)
   (let loop ((acc 0))  
       (if (eq? 'wall (do-action 'look))
      (begin (display "Found a wall\n") acc)
      (begin (do-action 'forward) (loop (add1 acc))))))

(define (turn-around) (do-action 'right) (do-action 'right))

(define (main)
  (tcp-init)
  (let* ((i (pace))
         (j (begin (turn-around) (run i) (do-action 'left) (pace))))
    (turn-around) (run j) (do-action 'right)
    (let loop ()
        ; home position, facing north
      (run i) (turn-around) (run i) (do-action 'left)
        ; home position, facing east
      (run j) (turn-around) (run j) (do-action 'right)
      (loop))))

