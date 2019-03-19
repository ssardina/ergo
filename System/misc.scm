#lang racket

;;; These are misc utilities used by Ergo

(require (lib "defmacro.ss"))
(provide define-macro apropos (for-syntax make-alist) iota
         for/append for/only sum-map product-map and-map or-map
         vector-set vector-set* open-tcp-client open-tcp-server
         plan-draw plan-act plan-next-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; this is useful for interactively locating functions etc
(define (apropos what)
  (let ((str (if (symbol? what) (symbol->string what) what)))
    (for ((sym (namespace-mapped-symbols)))
      (when (regexp-match str (symbol->string sym)) (displayln sym)))))

;; convert (f1 v1 ... fN vN) to ((f1 . v1) ... (fN . vN)) 
(define-for-syntax (make-alist alts)
  (cond
   ((null? alts) '())
   ((null? (cdr alts)) (error "odd number of elements in definition"))
   (else (cons (cons (car alts) (cadr alts)) (make-alist (cddr alts))))))

;; generate the numbers from 0 to n-1
(define (iota n) (build-list n values))

;; missing mapping function analogous to append-map
(define-macro (sum-map fn . args)
  (let ((vlast (gensym)) (vars (map (lambda (x) (gensym)) args)))
    `(foldl (lambda (,@vars ,vlast) (+ (,fn ,@vars) ,vlast)) 0 ,@args)))

;; missing mapping function analogous to append-map
(define-macro (product-map fn . args)
  (let ((vlast (gensym)) (vars (map (lambda (x) (gensym)) args)))
    `(foldl (lambda (,@vars ,vlast) (* (,fn ,@vars) ,vlast)) 1 ,@args)))

;; renamed mapping functions (analogous to append-map)
(define and-map andmap)
(define or-map ormap)

;; missing for/append function analogous to for/list for/sum etc.
(define-macro (for/append vl . body)
  (let ((v (gensym 'v)))
    `(append-map (lambda ,(map car vl) ,@body)
       ,@(for/list ((p vl))
          `(let ((,v ,(cadr p))) (if (list? ,v) ,v (iota ,v)))))))

;; new iterative function that uses filter (the way for/list uses map)
(define-macro (for/only vl . body)
    (let ((v (gensym 'v)))
      `(filter (lambda ,(map car vl) ,@body) 
       ,@(for/list ((p vl))
          `(let ((,v ,(cadr p))) (if (list? ,v) ,v (iota ,v)))))))
               
;; vector copy then set! 
(define-macro (vector-set vec ref val)
  (let ((new (gensym)))
    `(let ((,new (vector-copy ,vec))) (vector-set! ,new ,ref ,val) ,new)))

;; vector copy then set!*
(define-macro (vector-set* vec . args)  ; args = ref1 val1 ... refK valK
   (let ((new (gensym)))
     `(let ((,new (vector-copy ,vec))) (vector-set*! ,new ,@args) ,new)))

;; packaging around tcp-connect
(define (open-tcp-client portnum . rest)
  (let ((machine (if (null? rest) "localhost" (car rest))))
    (let-values ([(iport oport) (tcp-connect machine portnum)])
      (file-stream-buffer-mode iport 'none)         
      (file-stream-buffer-mode oport 'none)         
      (list iport oport))))

;; packaging around tcp-listen + tcp-accept
(define (open-tcp-server portnum . rest)
  (let* ((machine (if (null? rest) "localhost" (car rest)))
         (listen (tcp-listen portnum 1 #t machine)))
    (let-values ([(iport oport) (tcp-accept listen)])
      (file-stream-buffer-mode iport 'none)         
      (file-stream-buffer-mode oport 'none)         
      (list iport oport)))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Utilities used in files gendo.scm and ergo-genplan.scm

;;;  A plan is a list of transitions, each of the form (q q' a r)
;;;   where q, q' are states (integers), a is an action and
;;;         r is a sensing result or #t

;; draw a plan (for the freely available "dot" utility) in the file tmp.dot
(define (plan-draw plan)
  (when plan
    (when (file-exists? "tmp.dot") (delete-file "tmp.dot"))
    (eprintf "Writing plan in file tmp.dot\n")
    (with-output-to-file "tmp.dot"
      (lambda ()
        (printf "digraph{\n")
        (printf "  node [style=filled]\n")
        (printf "  edge [fontname=\"Courier-Bold\"]\n")
        (printf "  0 [label=STOP,peripheries=2]\n")
        (printf "  GO [label=\"\",height=0,width=0,peripheries=0]\n")
        (if (null? plan) (printf "  GO -> 0\n") (printf "  GO -> 1\n"))
        (for ((q (if (null? plan) 0 (apply max (map car plan)))))
          (printf "  ~a [label=\"~a\"]\n" (+ q 1) (plan-act (+ q 1) plan)))
        (for ((tr plan))
          (if (eq? (cadddr tr) #t)
              (printf "  ~a -> ~a\n" (car tr) (cadr tr))
              (printf "  ~a -> ~a [label=\"~a    \"]\n"
                      (car tr) (cadr tr) (cadddr tr))))
        (printf "{rank=min; GO}")
        (printf "}\n")))  ))

;; the act associated with state q in plan p (or #f if none)
(define (plan-act q p) (let ((tr (assq q p))) (and tr (caddr tr))))

;; the transition from state q and sensing result r in plan p (or #f if none) 
(define (plan-next-state q r p) 
  (if (null? p) #f
      (let ((tr (car p)))
        (if (and (= (car tr) q) (eq? (cadddr tr) r)) (cadr tr)
            (plan-next-state q r (cdr p))))))
