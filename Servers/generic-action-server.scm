;;; This program is a very simple action server over TCP.
;;  Clients send it actions and expect responses (perhaps exog actions). 
;;; Usage: racket -fm action-server.scm <portnum> [ <machine> ]
;;; the system then waits for TCP connections on <portnum>

;;; Three functions need to be defined for a robot or simulation:
;;;    (initialize)
;;;       set up the robot or similation
;;;    (service iport oport)
;;;       read actions on iport and produce responses on oport until EOF
;;;    (finalize) 
;;;       close down robot or simulation   

(define (main . strs)
  (when (null? strs) (error "Missing TCP portnum for action server"))
  (let* ((num (string->number (car strs)))
         (mach (if (null? (cdr strs)) "localhost" (cadr strs)))
         (listen (tcp-listen num 1 #t mach)))
    (eprintf "Waiting for ~a TCP connection on port ~a\n" mach num)
    (let-values ([(iport oport) (tcp-accept listen)])
      (file-stream-buffer-mode iport 'none)             
      (file-stream-buffer-mode oport 'none)             
      (eprintf "Client has connected.\n")
      (initialize)
      (call-with-exception-handler
       (lambda (any) (eprintf "Client has disconnected.\n") (exit 0))
       (lambda () (service iport oport)))
      (finalize))))
