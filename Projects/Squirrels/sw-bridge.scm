;;; This is interface code that can be used for any ERGO agent that interacts
;;;   with a Squirrel World server.

;; The SW interface parameters (change as necessary)
(define portnum 8123)                      ; port for SW server
(define machine "localhost")               ; machine for SW server
(define tracing? #f)                       ; print action info for debugging?

;;; Initializing the TCP connection to the SW server
(eprintf "Connecting to the Squirrel World\n") 
(define sw-ports (open-tcp-client portnum machine))
(define sw-name (read (car sw-ports)))     ; the first output of SW server
(eprintf "Squirrel ~a ready to go\n" sw-name)

(define sw-acts                            ; acts to send to SW server
  '(feel look smell listen left right forward pick drop eat build quit))
(define sw-responses (hasheq               ; exog responses for sensing acts
  'feel 'set-energy! 'look 'set-view! 'smell 'set-aroma! 'listen 'set-sound!))

;; Define the two ERGO interfaces (using a channel for exog actions)
(let ((chan (make-channel)) (iport (car sw-ports)) (oport (cadr sw-ports)))
  (define (sw-read) (channel-get chan))    ; get exog from sw-write (below)
  (define (sw-write act)                   ; send act over TCP and get response
    (and (memq act sw-acts)
      (let ()    
        (displayln act oport)
        (let ((ans (read iport)) (exog (hash-ref sw-responses act #f)))
          (and (eof-object? ans) (error "No response from Squirrel World"))
          (and tracing? (eprintf "Sending: ~a. Receiving: ~a\n" act ans))
          (and (eq? ans 'fail) (eprintf "Warning: Action ~a failed\n" act))
          (and exog (channel-put chan (list exog ans)))))))
  (define-interface 'in  sw-read)
  (define-interface 'out sw-write))
