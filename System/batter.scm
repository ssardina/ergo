#lang scheme

;;; This file defines all the functions to support BATs

(require "misc.scm")
(provide define-fluent define-fluents define-action define-states
         prereq-table effect-table sensing-table 
         current-state restore-state with-restore-to save-state-excursion
         get-states set-states with-states change-partition
         known? possible-values set-new-states
         belief sample-mean sample-variance sample-stats
         legal-action? sensing-value change-state
         display-execution)

(define-for-syntax (bat-err . args) (apply error 'BAT-ERROR args))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Fluents and States

;; a list of the fluent names
(define-for-syntax all-fluents '())
;; a list of reserved words (cannot be used as fluents)
(define-for-syntax keywords '(#:prereq #:sensing #:sequential?))
;; is this run for one state or for many states?
(define-for-syntax single-state? #f)

;; define a single fluent
(define-macro (define-fluent sym val)
  (unless (symbol? sym) (bat-err "~a is not a symbol" sym))
  (when (memq sym keywords) (bat-err "~a is a reserved name" sym))
  (when (memq sym all-fluents) (bat-err "~a is already used" sym))
  (set! all-fluents (cons sym all-fluents))
  `(define ,sym ,val))

;; main defining function for single state (complete knowledge)
(define-macro (define-fluents . args)
  (set! single-state? #t)
  (let* ((al (make-alist args)) (syms (map car al)) (vals (map cdr al)))
    `(begin
       (set-states #f)
       ,@(map (lambda (sym val) `(define-fluent ,sym ,val)) syms vals))))

;; save and restore state of fluents using a vector 
(define-macro (current-state) `(vector ,@all-fluents))
(define-macro (restore-state vec)
  (if (null? all-fluents) #f
      `(begin ,@(for/list ((i (length all-fluents)) (sym all-fluents))
                   `(set! ,sym (vector-ref ,vec ,i))))))

(define-macro (with-restore-to w . body)
  (let ((var (gensym)))
    `(let ((,var ,w)) (begin0 (begin ,@body) (restore-state ,var)))))

(define-macro (save-state-excursion . body)
  `(with-restore-to (current-state) ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Multiple initial states

(define all-states #f)
(define (set-states x) (set! all-states x))
(define (get-states) all-states)
(define-macro (with-states states . body)
  (let ((var (gensym)))
    `(let ((,var (get-states)))
        (set-states ,states)   
        (begin0 (begin ,@body) (set-states ,var)))))

;; main defining function for multiple states (incomplete knowledge)
(define-macro (define-states parms . args)
  (set! single-state? #f)
  (let* ((al (make-alist args))  (states (gensym 's))
         (al* (filter (lambda (p) (not (eq? (car p) '#:known))) al))
         (syms* (map car al*)) (vals* (map cdr al*))
         (known (or (assq '#:known al) (cons '#:known #t)))
         (syms (if (memq 'weight syms*) syms* (cons 'weight syms*)))
         (vals (if (memq 'weight syms*) vals* (cons 1.0 vals*))))
    `(begin
       ,@(for/list ((sym syms)) `(define-fluent ,sym (void)))
       (set-states
        (save-state-excursion
         (let ((,states '()))
           (for* ,parms
             ,@(for/list ((sym syms) (val vals)) `(set! ,sym ,val))
             (when ,(cdr known) (set! ,states (cons (current-state) ,states))))
           ,states))))))
            

;; evaluate fexpr in all-states 
(define-macro (known? fexpr . rest)
  (let ((s (gensym)) (states (if (null? rest) '(get-states) (car rest))))
    `(save-state-excursion
        (for/and ((,s ,states)) (restore-state ,s) ,fexpr))))

(define-macro (possible-values fexpr . rest)
  (let ((s (gensym)) (states (if (null? rest) '(get-states) (car rest))))
    `(save-state-excursion (remove-duplicates
        (for/list ((,s ,states)) (restore-state ,s) ,fexpr)))))

(define-macro (set-new-states fexpr . rest)
  (let ((s (gensym)) (states (if (null? rest) '(get-states) (car rest))))
    `(save-state-excursion (set-states (for/list ((,s ,states))
        (restore-state ,s) ,fexpr (current-state))))))

;; partition the list of next states after act a according to sensing results.
;; returns a list like ((r1 w1 w2) (r2 w3 w4 w5) (r3 w6 w7 w7 w8 s9))
(define-macro (change-partition fexpr . rest)
  (let* ((a (gensym)) (next `(begin (change-state ,a #f) (current-state)))
         (sts (if (null? rest) '(get-states) (car rest))))
    `(save-state-excursion
      (let ((,a ,fexpr))
        (if (hash-ref sensing-table (if (symbol? ,a) ,a (car ,a)) #f)
          (let ((h (make-hasheq)))
            (for ((w ,sts))
              (restore-state w)
              (let ((r (sensing-value ,a #f)))
                (hash-set! h r (cons ,next (hash-ref h r '())))))
            (hash->list h))
          (list (cons #t (for/list ((w ,sts)) (restore-state w) ,next))))))))

;; get weighted values for all expressions in list
(define-macro (sample-stats . fexprs)
  (let ((norm (gensym)) (s (gensym)) (al (for/list ((f fexprs)) (gensym))))
    `(let ((,norm 0.0) ,@(for/list ((a al)) `(,a 0.0)))
       (unless (get-states) (error "Sample states not yet initialized"))
       (save-state-excursion 
        (for ([,s (get-states)])
             (restore-state ,s)
             ,@(for/list ((a al) (f fexprs)) `(set! ,a (+ ,a (* weight ,f))))
             (set! ,norm (+ ,norm weight))))
       (if (= ,norm 0.0) (error "No samples for statistics")
           (list ,@(for/list ((a al)) `(/ ,a ,norm)))))))

(define-macro (sample-mean fexpr) `(car (sample-stats ,fexpr)))

(define-macro (sample-variance fexpr)
  (let ((v (gensym)))
    `(let ((,v (sample-stats ,fexpr (* ,fexpr ,fexpr))))
       (- (cadr ,v) (* (car ,v) (car ,v))))))

(define-macro (belief fexpr . rest)
  (if (null? rest) `(sample-mean (if ,fexpr 1.0 0.0))
      (let ((v (gensym)) (fexpr2 (car rest)))
        `(let ((,v (sample-stats (if (and ,fexpr ,fexpr2) 1.0 0.0)
                                 (if ,fexpr2 1.0 0.0))))
           (/ (car ,v) (cadr ,v))))))
                      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Action lookup for prereq, sensing, and effects

;; lookup tables for actions
(define prereq-table (make-hasheq))
(define sensing-table (make-hasheq))
(define effect-table (make-hasheq))

(define-macro (legal-action? a . rest) (process 'prereq-table a rest))
(define-macro (sensing-value a . rest) (process 'sensing-table a rest))
(define-macro (change-state a . rest) (process 'effect-table a rest))

;; lookup in table for action a and handle result, for single and multi
(define-for-syntax (process table a rest)
  (let ((single? (or single-state? (and (not (null? rest)) (not (car rest)))))
        (states (if (null? rest) '(get-states) (car rest)))
        (sym (gensym 'sym)) (args (gensym 'args)) (fn (gensym 'fn)))
    `(let* ((,sym (if (symbol? ,a) ,a (car ,a)))
            (,args (if (symbol? ,a) '() (cdr ,a)))
            (,fn (hash-ref ,table ,sym #f)))
       (or (not ,fn) ,(if single? `(apply ,fn ,args)
                          `(,(mproc table) (apply ,fn ,args) ,states))))))

;; handle table results for multiple initial states
(define-for-syntax (mproc table-name)
  (case table-name ((prereq-table) 'known?)
        ((sensing-table) 'possible-values)
        (else 'set-new-states)))
      
;; display a sequence of state changes given by act-list
;; displayer arg is a thunk that prints something for a state
(define-macro (display-execution displayer act-list)
  (let ((acts (gensym 'acts)))
    `(let ((,acts ,act-list))
       (if (not ,acts) (display "No list of actions given.\n")
           (save-state-excursion
             (display "    The first state is:\n") (,displayer)
             (for ((a ,acts))
               (printf "\n    Executing action ~a." a)
               (change-state a)
               (display "  The resulting state is:\n") (,displayer))
             (display "\n    That was the final state.\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;  Action definition

;; main defining function for an action
(define-macro (define-action pat . args)
  (unless (or (symbol? pat) (and (list? pat) (andmap symbol? pat)))
          (bat-err "Improper action pattern ~a" pat))
  (define act (if (symbol? pat) pat (car pat)))
  (define parms (if (symbol? pat) '() (cdr pat)))
  (define alist (make-alist args))
  (define pre (assq '#:prereq alist))
  (define sen (assq '#:sensing alist))
  (define mode (assq '#:sequential? alist))
  (define eff (filter (lambda (p) (not (memq (car p) keywords))) alist))
  (define vars (map (lambda (p) (gensym 'var)) eff))
  (for ((p eff)) (unless (memq (car p) all-fluents)
                         (bat-err "~a is not defined as a fluent" (car p))))
  `(begin
     (define ,act ,(if (symbol? pat) `',act `(lambda x (cons ',act x))))
     ,(when pre `(hash-set! prereq-table ',act (lambda ,parms ,(cdr pre))))
     ,(when sen `(hash-set! sensing-table ',act (lambda ,parms ,(cdr sen))))
     ,(when (not (null? eff))
        `(hash-set! effect-table ',act
            ,(if mode  ; what mode of fluent change?
                 `(lambda ,parms  ; sequential
                    ,@(for/list ((p eff)) `(set! ,(car p) ,(cdr p))))
                 `(lambda ,parms  ; parallel
                    (let ,(for/list ((v vars) (p eff)) (list v (cdr p)))
                      ,@(for/list ((v vars) (p eff))
                           `(set! ,(car p) ,v)))))))))

