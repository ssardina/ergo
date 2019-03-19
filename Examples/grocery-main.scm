;;; This is a version of a grocery shopping robot.
;;; The goal is to obtain all the items on a shopping list.

(include "grocery-bat.scm")

;; get to a given shelf on the current aisle: up or down or nothing
(define (get-to-shelf s)
  (:choose (:act (up! s)) (:test (= (shelf location) s))  (:act (down! s))))

;; get to a given aisle: get to the end of the aisle, then right or left
(define (get-to-aisle a)
  (:unless (= (aisle location) a)
      (:choose (get-to-shelf MIN-SHELF) (get-to-shelf MAX-SHELF))
      (:choose (:act (right! a)) (:act (left! a))) ))

;; get to a goal location: get to that aisle and then to that shelf
(define (get-to-location goal)
  (:begin (get-to-aisle (aisle goal)) (get-to-shelf (shelf goal))))

;; the robot procedure: pick up every item and then go to checkout
(define (get-groceries)
   (:begin
      (:until (null? grocery-list)
         (:for-some item grocery-list
            (get-to-location (grocery-loc item))
            (:act (pickup! item)) ))
      (get-to-location CHECK-OUT-LOCATION)  ))

;; the main program
(define (main) (display-execution show-store (ergo-do (get-groceries))))
