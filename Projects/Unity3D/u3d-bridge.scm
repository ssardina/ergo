;;;  This program provides a bridge between Scheme and a Unity 3d program.

(define machine "localhost")      ; machine running the Unity 3d server

;; make connection over TCP 
(define (u3d-start-comm portnum u3d-tracing?)
  (define (ido x) (and u3d-tracing? (printf ">>> ~a\n" x)) x)
  (define (idi x) (and u3d-tracing? (printf "<<< ~a\n" x)) x)
  (eprintf "Connecting to the Unity 3d Engine\n")
  (define u3d-ports (open-tcp-client portnum machine))
  (eprintf "Unity 3d is read to go\n")
  (define-interface 'out (lambda (act) (displayln (ido act) (cadr u3d-ports))))
  (define-interface 'in (lambda () (idi (read (car u3d-ports))))))

