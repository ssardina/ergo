;;; This is a version of the elevator domain, formalized as in Ray Reiter's
;;; original in Golog, where the actions take numeric arguments.

;; The basic action theory: two fluents and three actions 
(define-fluents  
   floor 7                    ; where the elevator is located
   on-buttons '(3 5))         ; the list of call buttons that are on 

(define-action (up n)         ; go up to floor n
   #:prereq  (< floor n)
   floor n)

(define-action (down n)       ; go down to floor n
   #:prereq  (> floor n)
   floor n)

(define-action (turnoff n)    ; turn off the call button for floor n
   on-buttons (remove n on-buttons))

;; Get to floor n using an up action, a down action, or no action
(define (go-floor n)
   (:choose (:act (up n)) (:test (= floor n)) (:act (down n))))

;; Serve all the floors and then park
(define (serve-floors)
   (:begin 
      (:until (null? on-buttons)
         (:for-some n on-buttons (go-floor n) (:act (turnoff n))))
      (go-floor 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Main program: run the elevator using the above procedure

(define (main) (display (ergo-do #:mode 'first (serve-floors))))
