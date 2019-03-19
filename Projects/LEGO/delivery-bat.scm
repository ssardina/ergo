;;;  This the BAT for a LEGO delivery robot

(include "delivery-map.scm")

(define-fluents
  location    1      ; where the robot was last located on the map
  orientation 1      ; 0,1,2,3 according to the last orientation of the robot
  in-transit? #f     ; is the robot between locations?
  loading?    #f     ; is the robot loading/unloading packages?
  onboard     '()    ; a list of (pkg from to) items that are with the robot
  pending     '()    ; a list of (pkg from to) items that are awaiting pickup
  done        '())   ; a list of (pkg from to) items that have been delivered 

;; the endogenous actions
(define-action (leave-location! r)
  #:prereq    (and (not in-transit?) (next-location location orientation))
  in-transit? #t)

(define-action (turn! r dir)
  #:prereq    (not in-transit?)
  orientation (shift-orientation orientation dir))

(define-action (req-customer-action! r)
  #:prereq    (not in-transit?)
  loading?    #t)

;; the exogenous actions
(define-action (request-delivery! u obj from to)
  pending     (append pending (list (list obj from to))))

(define-action (arrive-at-location! r)
  location    (next-location location orientation)
  orientation (next-orientation location orientation)
  in-transit? #f)

(define (pkgs-on) (for/only ((x pending)) (= (cadr x) location)))
(define (pkgs-off) (for/only ((x onboard)) (= (caddr x) location)))

(define-action (customer-action-done! r)
  loading?    #f
  done        (append (pkgs-off) done)
  onboard     (append (remove* (pkgs-off) onboard) (pkgs-on))
  pending     (remove* (pkgs-on) pending))
