;;; This is interface code that can be used for an ERGO agent that
;;;   uses tagged actions for online interactions 

;; this calls define-interface after modifying readfn and printfn to use tag
(define (define-tagged-interfaces tag readfn printfn)
  (define (read-add-tag)
    (let ((r (readfn)))
      (if (symbol? r) (list r tag) (cons (car r) (cons tag (cdr r))))))
  (define (print-detag a)
    (and (not (symbol? a)) (not (null? (cdr a))) (eq? (cadr a) tag)
         (printfn (cons (car a) (cddr a)))))
  (define-interface 'in read-add-tag)
  (define-interface 'out print-detag))

;; setup in and out interfaces over TCP
(define (tag-tcp-setup tag portnum IPaddress)
  (eprintf "Setting up interfaces over TCP for ~a\n" tag) 
  (define tcp-ports (open-tcp-client portnum IPaddress))
  (define-tagged-interfaces tag
    (lambda () (read (car tcp-ports)))
    (lambda (act) (displayln act (cadr tcp-ports))))
  (eprintf "~a is ready to go\n" tag))

;; setup in and out interfaces with standard IO
(define (tag-stdio-setup)
  (eprintf "Setting up interfaces over stdin and stdout\n")
  (define-tagged-interfaces 'user read-exogenous write-endogenous))
