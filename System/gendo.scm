;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file is loaded on demand with (ergo-gendo ...)
;;; The top level program is ergo-gendo-fn 

;; the main function used by :act and :test
;; this version makes a history of the form (act (r thunk) act (r thunk) ...)
(define (ergo-trans a h fail succ)
  (define (make-swl a wl)
    (sort (change-partition a wl) (lambda (x y) (> (length x) (length y)))))
  (if (not a) (succ h fail #f)
      (if (not (legal-action? a)) (fail)
          (let ((wl (get-states)))
            (define (fail*) (with-states wl (fail)))
            (let loop ((swl (make-swl a wl)))
              (define thk (if (null? (cdr swl)) #f
                              (lambda () (loop (cdr swl)))))
              (define h* (cons (list (caar swl) thk) (cons a h)))
              (with-states (cdar swl) (succ h* fail* #f)))))))

;; the top-level function  ...  closing paren is at the end
(define (ergo-gendo-fn pgm #:draw? [draw? #t] #:states [max-qstates 40])

;; convert the chains chs into an FSA plan and call succ, or call fail
;; Args q, n, p are as with plan-all in genplan.scm
(define (make-plan q n p chs maxq fail succ)
  (define (all-qstates n) (iota (+ n (if (< n maxq) 2 1))))
  (define (subchs r chs) (map cddr (for/only ((ch chs)) (eq? (cadr ch) r))))
  (define (sense-results chs) (remove-duplicates (map cadr chs)))
  (define (improper-end? q chs)
    (or (and (= q 0) (not (and-map null? chs)))
        (and (> q 0) (or-map null? chs))))
  (define (wrong-first-act? a q p)
    (let ((a* (plan-act q p))) (and a* (not (equal? a a*)))))
  (define (all-rs rs n p a fail)
    (if (null? rs) (succ n p fail)
        (let* ((r (car rs)) (q* (plan-next-state q r p)) (chs* (subchs r chs)))
          (define (succ* n* p* fail*) (all-rs (cdr rs) n* p* a fail*))
          (if q* (make-plan q* n p chs* maxq fail succ*)
              (let loop ((ql (all-qstates n)))
                (if (null? ql) (fail)
                    (let* ((q* (car ql)) (trans (list q q* a r))
                           (n* (max n q*)) (p* (cons trans p)))
                      (define (fail*) (loop (cdr ql)))
                      (make-plan q* n* p* chs* maxq fail* succ*))))))))
  (if (improper-end? q chs) (fail)
      (if (= q 0) (succ n p fail)
          (let* ((acts (map car chs)) (a (car acts)))
            (if (wrong-first-act? a q p) (fail)
                (all-rs (sense-results chs) n p a fail))))))

;; make a plan from a list of chains by iterative deepening
(define (top-make-plan chains)
  (define (fail) #f)
  (define (succ n p f) p)
  (if (and-map null? chains) '()
      (for/or ((maxq max-qstates)) (make-plan 1 1 '() chains maxq fail succ))))

;; the success continuation for ERGO programs 
(define (top-succ h fail c)
  (define (compat? ch1 ch2)
    (if (null? ch1) (null? ch2)
        (and (not (null? ch2)) (equal? (car ch1) (car ch2))
             (or (not (eq? (cadr ch1) (cadr ch2)))
                 (compat? (cddr ch1) (cddr ch2))))))
  (define (first-chain h)
    (let loop ((h h) (ch '()))
      (if (null? h) ch
          (loop (cddr h) (cons (cadr h) (cons (caar h) ch))))))
  (define (other-chains h)
    (if (null? h) '()
        (let ((thk (cadar h)))
          (if (not thk) (other-chains (cddr h)) (thk)))))
  (if c (c h fail top-succ)
      (let ((first (first-chain h)) (chs (other-chains h)))
        (if (or (not chs) (not (for/and ((ch chs)) (compat? first ch))))
            (fail)
            (cons first chs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the body of ergo-gendo

;; begin is atomic initially
(ergo-begin-atomic! #t)

;; only works when "define-states" has been used 
(when (not (get-states)) (error "States have not been initialized"))

;; get chains from running program, then convert to plan
(let ((start (current-milliseconds))
      (chains (pgm '() (lambda () #f) top-succ)))
  (if (not (list? chains)) (eprintf "Program was not successful\n")
      (eprintf "Program produced ~a chains\n" (length chains)))
  (let ((plan (and (list? chains) (top-make-plan chains))))
    (when plan
      (eprintf "Plan found after ~a ms.\n" (- (current-milliseconds) start))
      (when (and draw? (not (null? plan))) (plan-draw plan)))
    plan))
) ; the closing paren for ergo-gendo

