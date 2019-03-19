;;; This the main program for a LEGO delivery robot 
;;; To start, first get EV3 server running at IP address <addr>
;;; Then do:  racket -l ergo -f <this file>  -m <addr>

(include "tag-bridge.scm")
(include "delivery-bat.scm")

(define home-location location)
(define robo 'my-EV3)

(define (at-package-loc?) (not (and (null? (pkgs-on)) (null? (pkgs-off)))))

(define (load/unload)
  (:begin (:act (req-customer-action! robo)) (:while loading? (:wait))))

(define (work-to-do?) (not (and (null? onboard) (null? pending))))

(define (next-work-loc)
  (let ((loc1 (if (null? onboard) #f (caddr (car onboard))))
        (loc2 (if (null? pending) #f (cadr (car pending)))))
    (if (not loc1) loc2
        (if (not loc2) loc1
            (let ((p1 (find-path location loc1)) (p2 (find-path location loc2)))
              (if (> (length p1) (length p2)) loc1 loc2))))))

(define (head-for loc goal)
  (:let ((dir (get-direction loc orientation (cadr (find-path loc goal)))))
    (:when dir (:act (turn! robo dir)))
    (:act (leave-location! robo))
    (:while in-transit? (:wait))))

(define (delivery)
  (:monitor
    (:when (at-package-loc?) (load/unload))
    (:while (work-to-do?) (head-for location (next-work-loc)))
    (:until (eq? location home-location) (head-for location home-location))
    (:while #t (:wait))))

(define (main . args)
  (and (null? args) (error "Must supply an IP address for EV3"))
  (tag-stdio-setup)                          ; for request-delivery! action
  (tag-tcp-setup robo 8123 (car args))       ; port 8123 assumed
  (ergo-do #:mode 'online (delivery)))
