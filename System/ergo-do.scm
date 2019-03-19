;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This file is loaded on demand with (ergo-do ...)
;;; The top level program is ergo-do-fn 
;;; The optional parameter #:mode is
;;;      'offline  -- offline, prompt for keep or discard
;;;      'first    -- offline, return first legal execution (default)
;;;      'count    -- offline, return number of legal executions
;;;      'online   -- online

;; the function used by :act and :test, set in with-online-xeq with-offline-xeq
(define ergo-trans (void))

;;  To block and unblock waiting for an exog act via :wait
(define exog-wait-chan (make-channel)) 
(define (ergo-wait-for-exog . args)
  (if (null? args) (channel-put exog-wait-chan #t)
      (sync/timeout (car args) (channel-put-evt exog-wait-chan #t))))
(define (unwait-for-exog) (channel-try-get exog-wait-chan))

;; the top-level function  ...  body at end
(define (ergo-do-fn pgm #:mode [mode 'first])  ; closing paren on last line

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transfer of actions to and from the outside world via threads

(define mon-endo-chans '())                    ; for syncing with monitors
(define mon-exog-chans '())                    ; for syncing with monitors

;; print error message and abort execution
(define (ergo-abort x)
  (eprintf "~a\nAborting after online error..." (exn-message x))
  (sleep 1) (exit 0))
  
;; return a function that behaves like fn, but that traps errors and aborts
(define (trap-errors msg fn)
  (lambda args
    (call-with-exception-handler
     (lambda (x) (eprintf "~a\n" msg) (ergo-abort x))
     (lambda () (apply fn args)))))

;; handle one exog interface:  call given reader and put results on chan
(define (run-exog-interface ifc chan)
  (let ((reader (trap-errors "Error getting exogenous action" (ifc))))
    (let loop ()
      (let ((a (reader)))
        (when a (unwait-for-exog) (channel-put chan a) (sleep)))
      (loop))))

;; handle one endo interface:  get act from chan and call given writer on it
(define (run-endo-interface ifc chan)
  (let ((writer (trap-errors "Error sending endogenous action" (ifc))))
    (let loop ()
      (let ((a (channel-get chan))) (when a (writer a) (sleep .1)))
      (loop))))
          
;;  Start threads for each declared interfaces
(define (start-ergo-monitors)
  (eprintf "Starting monitors for ~a exogenous and ~a endogenous interfaces.\n"
           (length ifc-exog-acts) (length ifc-endo-acts))
  (set! mon-exog-chans (map (lambda (ifc) (make-channel)) ifc-exog-acts))
  (set! mon-endo-chans (map (lambda (ifc) (make-channel)) ifc-endo-acts))
  (for ((ifc ifc-exog-acts) (chan mon-exog-chans))
    (thread (lambda () (run-exog-interface ifc chan))))
  (for ((ifc ifc-endo-acts) (chan mon-endo-chans))
    (thread (lambda () (run-endo-interface ifc chan)))))

;;  Stop all the threads that handle the interfaces
(define (stop-ergo-monitors)
  (for-each (lambda (chan) (channel-put chan #f)) mon-endo-chans)  ; pause
  (eprintf "Stopping all interface monitors.\n"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Transitions: online and offline

;; make the transition but keep the fail, allowing for backup!
(define (ergo-trans-offline a old-hist fail succ) 
  (if (not a) (succ old-hist fail #f)
      (if (not (legal-action? a)) (fail)
          (begin (change-state a) (succ (cons a old-hist) fail #f)))))

;; For online, handle all exog actions, handle given action, and then
;; make the state transition and commit by using online-fail as continuation
(define (ergo-trans-online a old-hist fail succ)
    (let loop ((exog (ormap channel-try-get mon-exog-chans)))
      (when exog 
            (change-state exog)
            (loop (ormap channel-try-get mon-exog-chans))))
    (if a 
        (if (not (legal-action? a)) (fail)
            (begin (for ((chan mon-endo-chans)) (channel-put chan a))
                   (sleep) (change-state a)
                   (succ old-hist online-fail #f)))
        (succ old-hist online-fail #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; What to do as programs start or end

;; the normal last resort failure: just return #f
(define (offline-fail) #f)

;; the last resort offline final: return history of actions (reversed)
(define (just-hist hist fail) (reverse hist))

;; the normal online failure: return #f
(define (online-fail) (eprintf "WARNING: Online program failure\n") #f)

;; the last resort online final: just end 
(define (online-done hist fail) (eprintf "Online program complete\n"))

;; the normal offline final: maybe return list of actions (reversed)
(define (try-final hist fail)
  (let ((rhist (reverse hist)))
    (printf "\n~a\n" rhist)
    (if (eq? (ergo-read #"OK? (y or n) ") 'y) rhist (fail))))
  
;; all interpreters start here
(define (common-do pgm fail final)
  (define (succ h f c) (if c (c h f succ) (final h f)))
  (pgm '() fail succ))

;; a variant of common-do for counting executions
(define (counting-do pgm)
  (let ((m 0))
    (common-do pgm offline-fail (lambda (h f) (set! m (+ 1 m)) (f)))
    m))

;; set transition for offline then evaluate 
(define-macro (with-offline-xeq expr)
  (let ((trans (gensym 'trans)))
    `(let ((,trans ergo-trans))
       (set! ergo-trans ergo-trans-offline)
       (begin0 ,expr (set! ergo-trans ,trans)))))

;; set transition and online monitors then evaluate
(define-macro (with-online-xeq expr)
  (let ((trans (gensym 'trans)))
    `(let ((,trans ergo-trans))
       (set! ergo-trans ergo-trans-online)
       (start-ergo-monitors)
       (begin0 (call-with-exception-handler ergo-abort 
                 (lambda () ,expr (stop-ergo-monitors)))
               (set! ergo-trans ,trans)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the body of ergo-do-fn

(unless (and (procedure? pgm) (= (procedure-arity pgm) 3))
  (error "Argument to ergo-do is not an ERGO program"))
(ergo-begin-atomic! #t)   ; initialize begin sequence to atomic
(save-state-excursion (case mode
    ((offline) (with-offline-xeq (common-do pgm offline-fail try-final)))
    ((first) (with-offline-xeq (common-do pgm offline-fail just-hist)))
    ((online) (with-online-xeq (common-do pgm online-fail online-done)))
    ((count) (with-offline-xeq (counting-do pgm)))
    (else (error "~a is incorrect mode of execution" mode))))
); ergo-do-fn
