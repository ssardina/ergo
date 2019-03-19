;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A simple sequential planner, loaded on demand by (ergo-simplan . args)
;;;  Parameters:
;;;     goal  =  predicate to be achieved
;;;     acts  =  list of allowable actions (or a function with this value)
;;;  Optional parameters
;;;     steps =  max number of steps in a plan (default 30)
;;;     loop? =  check for repeating states (default #f)
;;;     prune =  function for pruning (default #f)

(define (ergo-simplan-fn goal acts #:steps [steps 30] 
                 #:loop? [loop? #f] #:prune [prune (lambda () #f)])
  (define actfn (if (list? acts) (lambda () acts) acts))
  (define inis (if (not (get-states)) '() (cdr (get-states))))
  (define (check-inis h*)   ; return true when inis=()
    (for/and ((w inis)) 
      (restore-state w)
      (and (for/and ((a h*)) (and (legal-action? a #f) (change-state a #f)))
           (goal))))
  (define (simp* n h l)
    (define cur (current-state))
    (if (= n 0) (and (goal) (let ((h* (reverse h))) (and (check-inis h*) h*)))
        (and (not (and loop? (member cur l)))
             (not (prune))
             (for/or ((a (actfn)))
               (and (legal-action? a #f)       
                 (with-restore-to cur
                   (change-state a #f)
                   (simp* (- n 1) (cons a h) (if loop? (cons cur l) l))))))))
  (define (planloop start n)
    (let ((p (save-state-excursion (simp* n '() '()))))
      (when p (printf "Plan found after ~a ms.\n" 
                      (- (current-milliseconds) start)))
      (or p (and (< n steps) (planloop start (+ n 1))))))

  (unless (and (procedure? goal) (= (procedure-arity goal) 0))
   (error "Argument 1 to ergo-simplan must be a function of no arguments"))
  (unless (and (procedure? actfn) (= (procedure-arity actfn) 0))
   (error "Argument 2 to ergo-simplan must be a list or nullary function"))
  (unless (and (procedure? prune) (= (procedure-arity prune) 0))
   (error "Prune for ergo-simplan must be a function of no arguments"))
  (let ((start (current-milliseconds)))
    (if (not (get-states)) (planloop start 0)
        (save-state-excursion
         (restore-state (car (get-states))) (planloop start 0)))))
