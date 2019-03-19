#lang racket
(require (lib "defmacro.ss"))
(require "batter.scm")
;; these are the main program primitives
(provide :nil :fail :test :if :act :>> :<< ::act ::test
         :wait :let :begin :atomic :unless :when
         :choose :star :for-all :for-some :until :while
         :conc :monitor :search)
;; these are the auxilliary ones
(provide :let* :begin2 :monitor2 :conc2 :atomic-seq2 :monitor* :conc*
         :atomic* :divisible-seq2 ergo-begin-atomic!)

;;; An ERGO program <pgm> is one of the following:
;;;     :nil
;;;     :fail
;;;     (:>> fexpr ... fexpr) or (:<< fexpr ... fexpr)
;;;     (:test <boolean-fexpr>)
;;;     (:act <act-fexpr>)
;;;     (:let ((<var> <fexpr>) ... (<var> <fexpr>)) <pgm> ... <pgm>)
;;;     (:if <boolean-fexpr> <pgm1> <pgm2>)
;;;     (:when <boolean-fexpr> <pgm1> ... <pgm>) or (:unless ...)
;;;     (:begin <pgm> ... <pgm>)
;;;     (:while <boolean-fexpr> <pgm> ... <pgm>) or (:until ...)
;;;     (:choose <pgm> ... <pgm>)
;;;     (:star <pgm> ... <pgm>)
;;;     (:for-all <var> <list-fexpr> <pgm> ... <pgm>)
;;;     (:for-some <var> <list-fexpr> <pgm> ... <pgm>)
;;; and the two concurrency constructs
;;;     (:conc <pgm> ... <pgm>)
;;;     (:monitor <pgm> ... <pgm>)
;;; where an expression <fexpr> can use fluents as global variables
;;;
;;; The online version has one extra construct
;;;     (:search <pgm> ... <pgm>)  

;;; Each program construct evaluates to a function
;;;     (lambda (hist fail succ) ...) where 
;;;        hist is a list of actions (when offline, else empty)
;;;        fail is a function (lambda () ...)
;;;        succ is a function (lambda (hist fail cont) ...)
;;;          where cont is either #f (no continuation) or an ERGO program

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Utilities

;;  Expands to a call of define-macro with new variables for the three
;;    variables hist, fail, and succ and a spare one, var, in case.
(define-macro (dm-ergo pat . exprs)
   (let ((letl '((fail (gensym)) (hist 'current-history)
                 (succ (gensym)) (var (gensym))))
         (laml '(,hist ,fail ,succ)))
     `(define-macro ,pat 
        (let ,letl ,(list 'quasiquote `(lambda ,laml ,@exprs))))))

;;  Expands to define-macro that unfolds fn-many to nested fn-two calls
;;  e.g (:begin p1 p2 p3 p4) => (:begin2 p1 (:begin2 p2 (:begin2 p3 p4)))
(define-macro (dm-unfold fn-many fn-two)
   `(define-macro (,fn-many . body) (if (null? body) ':nil
       (let loop ((pgms body))
         (if (null? (cdr pgms)) (car pgms)
             (list ',fn-two (car pgms) (loop (cdr pgms))))))))

;;  A success function that is like the given succ but uses a new 
;;  continuation that is composed with p using the given compfn 
(define (succ+ succ compfn p) 
  (lambda (hist fail cont) (succ hist fail (if (not cont) p (compfn cont p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  The basic language constructs

(define :nil                            ; always succeed
  (lambda (hist fail succ) (succ hist fail #f)))

(define :fail                           ; always fail
  (lambda (hist fail succ) (fail)))

(dm-ergo (:test cond)                   ; make a transition or fail
   (if ,cond                            ; depending on truth of condition
       (ergo-trans #f ,hist ,fail ,succ) 
       (,fail)))

(dm-ergo (:if cond p1 p2)               ; do p1 or p2 according to cond
   (if ,cond (,p1 ,hist ,fail ,succ) (,p2 ,hist ,fail ,succ)))

(dm-ergo (:act fexpr)                   ; make a transition or fail
  (ergo-trans ,fexpr ,hist ,fail ,succ))   ; depending on legality

(dm-ergo (:>> . body)                   ; eval body for effect then continue
   ,@body (,succ ,hist ,fail  #f))

(dm-ergo (:<< . body)                   ; same, but on failure only
   (,succ ,hist (lambda () ,@body (,fail)) #f))

(define-macro (::act fexpr)
  `(:begin (:<< (eprintf "Failure at action ~a\n" ,fexpr))
           (:act ,fexpr)
           (:>> (eprintf "Success at action ~a\n" ,fexpr))))

(define-macro (::test cond)
  `(:begin (:<< (eprintf "Failure at test of ~a\n" ',cond))
           (:test ,cond)
           (:>> (eprintf "Success at test of ~a\n" ',cond))))

(dm-ergo (:wait . args)                   ; wait for exogenous then (:test #t)
   (ergo-wait-for-exog ,@args) (ergo-trans #f ,hist ,fail ,succ))

(define-macro (:let . args)             ; do body after binding vars  
   (if (symbol? (car args)) `(:let* ,@args) `(:let* ,(gensym) ,@args)))

(dm-ergo (:let* name pairs . body)
   (let ()
     (define ,name (lambda ,(map car pairs) (:begin ,@body)))
     ((,name ,@(map cadr pairs)) ,hist ,fail ,succ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Sequence:  atomic (non-interruptable) and divisible (interruptable)

;; p1 and p2 in sequence with no interrupts until during p2
(define (:atomic-seq2 p1 p2)
  (lambda (hist fail succ)
    (p1 hist fail 
       (lambda (h f c) ((if (not c) p2 (:atomic-seq2 c p2)) h f succ)))))

;; p1 and p2 in sequence with interrupts during p1
(define (:divisible-seq2 p1 p2)
  (lambda (hist fail succ) 
    (p1 hist fail (succ+ succ :divisible-seq2 p2))))

(define :begin2 :atomic-seq2)    ; by default :begin is atomic

;; change whether :begin is atomic or divisible
(define (ergo-begin-atomic! flag)
  (set! :begin2 (if flag :atomic-seq2 :divisible-seq2)))

(dm-unfold :begin :begin2)

(dm-unfold :atomic* :atomic-seq2)

(define-macro (:atomic . body) `(:atomic* ,@body :nil))

(define-macro (:unless cond . body) `(:if ,cond :nil (:begin ,@body)))

(define-macro (:when cond . body) `(:unless (not ,cond) ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Basic non-determinism

(define-for-syntax (try-first w progs hist fail succ)
  (list (car progs) 
    hist 
    (if (null? (cdr progs)) fail
        `(lambda () (restore-state ,w)
                ,(try-first w (cdr progs) hist fail succ)))
    succ))

(dm-ergo (:choose . body)
   (let ((,var (current-state))) ,(try-first var body hist fail succ)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Iteration

(define-macro (:until cond . body) 
  (let ((loopvar (gensym)))
    `(let ,loopvar () (:unless ,cond ,@body (,loopvar)))))

(define-macro (:while cond . body) `(:until (not ,cond) ,@body))

;; ;; this is a DFS version of :star
;; (define-macro (:star . body)
;;   (let ((loopvar (gensym)))
;;     `(let ,loopvar () (:choose :nil (:begin ,@body (,loopvar))))))

;; this is a BFS version of :star
(define-macro (:star . body)
  (let ((loopvar (gensym 'loop)) (ivar (gensym)) (jvar (gensym)))
    `(let ,loopvar ((,ivar 0))
        (:choose (:for-all ,jvar ,ivar ,@body) (,loopvar (+ ,ivar 1))))))

(define-macro (:for-all var fexpr . body)
  (let ((loopvar (gensym 'loop)) (fvar (gensym)) (xvar (gensym)))
    `(:let ,loopvar
       ((,fvar (let ((,xvar ,fexpr)) (if (list? ,xvar) ,xvar (iota ,xvar)))))
       (:unless (null? ,fvar)
          (:let ((,var (car ,fvar))) ,@body (,loopvar (cdr ,fvar)))))))

(define-macro (:for-some var fexpr . body)
  (let ((loopvar (gensym 'loop)) (fvar (gensym)) (xvar (gensym)))
    `(:let ,loopvar
       ((,fvar (let ((,xvar ,fexpr)) (if (list? ,xvar) ,xvar (iota ,xvar)))))
       (:if (null? ,fvar) :fail
            (:let ((,var (car ,fvar))) 
                  (:choose (:begin ,@body) (,loopvar (cdr ,fvar))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Concurrency

;; set :begin2 to divisble for entire ergo-do run, then do conc*
(dm-ergo (:conc . pgms)
   (begin (ergo-begin-atomic! #f) ((:conc* ,@pgms) ,hist ,fail ,succ)))
          
(dm-unfold :conc* :conc2)         ; expand to nested calls of :conc2

(define (:conc2 p1 p2)
  (let loop ((b1 p1) (b2 p2))
    (lambda (hist fail succ)
      (define (c1 hist fail succ) (b1 hist fail (succ+ succ loop b2)))
      (define (c2 hist fail succ) (b2 hist fail (succ+ succ loop b1)))
      ((:choose c1 c2) hist fail succ))))

;; set :begin2 to divisble for entire ergo-do run, then do monitor*
(dm-ergo (:monitor . pgms)
   (begin (ergo-begin-atomic! #f) ((:monitor* ,@pgms) ,hist ,fail ,succ)))

(dm-unfold :monitor* :monitor2)   ; expand to nested calls of :monitor2

;;  Call p2 but with p1 (normally :when or :while) before every transition
(define (:monitor2 p1 p2)
   (define (mon-rev x y) (:monitor2 y x))
   (define (p2+ hist fail succ) 
     (p2 hist fail (succ+ succ mon-rev p1)))
   (:begin p1 p2+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Offline searching

;; find seq of acts to perform using ergo-do offline, then xeq them 
(define-macro (:search . body)
  (let ((var (gensym 'var)) (act (gensym 'act)))
    `(:let ((,var (ergo-do #:mode 'first (:begin ,@body))))
       (:test ,var) (:for-all ,act ,var (:act ,act)))))

