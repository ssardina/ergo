;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;;  A general planner, loaded on demand by (ergo-genplan . args)

;;  This is the planner front end. The function plan-all below does the work
(define (ergo-genplan-fn goal acts            ;; closing paren on last line
            #:states [states 10] #:deep? [deep? #t]
            #:steps [steps 30] #:prune [prune (lambda () #f)]
            #:loop? [loop? #f]  #:draw? [draw? #t] #:loose? [loose? #t])

;;  Check for repeating config in history h
(define (visited-config? q wl h) (member (list q wl) h))
(define (add-hist-config q wl h) (cons (list q wl) h))

;;  Check for repeating wl in history h
(define (visited-bstate? q wl h) (and (not (null? h)) (member wl (cdr h))))
(define (add-hist-bstate q wl h) (cons wl h))

;; Global default values
(define visited? visited-config?)                 ; configs for loop checking
(define add-hist add-hist-config)                 ; configs for loop checking
(define (loosefn x y) (> (length x) (length y)))  ; least constrained first

;;  Other global vars
(define max-qstates 0)         ; how many states allowed in plan
(define plan-found #f)         ; the plan that has been found
(define candidates 0)          ; total number of plans considered

;;  The planner itself: backtracking, depth-first, iterative deepening
;;  A bit hairy, but plan* is the inner loop and calls succ or fail
;;    q is plan state; p is plan; n is current max state in plan; h is history;
;;    succ is (lambda (p n fail) ...); fail is (lambda () ...)
(define (plan-all goal acts wl d)
  (define (all-qstates n) (iota (+ n (if (< n max-qstates) 2 1))))
  (define (make-swl a wl) (sort (change-partition a wl) loosefn))
  (define (call-acts wl) (with-states wl (acts)))
  (define all-actfn (if (list? acts) (lambda (wl) acts) call-acts))
  (define (plan* q wl p n d h succ fail)
    (define (do-act a fail)
      (define (all-sw swl p n d h fail)
        (if (null? swl) (succ p n fail)
            (let ((r (caar swl)) (wl* (cdar swl)))
              (define (succ* p n fail) (all-sw (cdr swl) p n d h fail))
              (define (do-q q* fail)
                (let* ((tr (list q q* a r)) (n* (max n q*))
                       (p* (if (member tr p) p (cons tr p))))
                  (plan* q* wl* p* n* d h succ* fail)))
              (set! candidates (+ candidates 1))  ; for stats
              (get-one do-q (plan-next-state q r p) (all-qstates n) fail))))
      (if (not (legal-action? a wl)) (fail)
          (all-sw (make-swl a wl) p n (- d 1) (add-hist q wl h) fail)))
    (if (or (null? wl) (= q 0)) (if (known? (goal) wl) (succ p n fail) (fail))
        (if (or (prune) (= d 0) (visited? q wl h)) (fail)
            (get-one do-act (plan-act q p) (all-actfn wl) fail))))
  (plan* 1 wl '() 1 d '() (lambda (p n fail) p) (lambda () #f)))

;; if x is not #f, commit to (fn x fail); else try it on some member of xs 
(define (get-one fn x xs fail)
  (if x (fn x fail)
      (let loop ((l xs))
        (if (null? l) (fail) (fn (car l) (lambda () (loop (cdr l))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Body of ergo-genplan-fn

(when (not (get-states)) (error "States have not been initialized"))

(unless loose? (set! loosefn (lambda (x y) (< (length x) (length y)))))
(when loop? (set! visited? visited-bstate?) (set! add-hist add-hist-bstate))

(let ((start (current-milliseconds)))
  (eprintf "Searching for a plan of size ")
  (let loop ((st (if deep? 1 states)))
    (unless (or plan-found (> st states))
      (set! max-qstates st)      
      (eprintf "~a " st) (flush-output)
      (set! plan-found (plan-all goal acts (get-states) steps))
      (loop (+ st 1))))
  (eprintf "\nConsidered ~a sensing outcomes from actions\n" candidates)
  (when plan-found
    (eprintf "Plan found after ~a ms.\n" (- (current-milliseconds) start))
    (when draw? (plan-draw plan-found)))
  plan-found)
)  ; closing for ergo-genplan-fn

