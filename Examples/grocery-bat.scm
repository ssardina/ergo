;;;  Basic action theory for a grocery world

;; Fixed world parameters
(define MIN-SHELF 0)                   ; min shelf on an aisle
(define MAX-SHELF 11)                  ; max shelf on an aisle
(define CHECK-OUT-LOCATION '(0 0))     ; location of checkout counter
(define STORE-LOCATION                 ; locations of items on shelves
   (hasheq 'milk '(2 4) 'chocolate '(7 8) ))

;; Fluents:  location of robot, desired items, contents of grocery cart
(define-fluents  
  location  '(0 0)
  grocery-list  '(milk chocolate)
  trolley-contents  '())

;; Auxilliary functions
(define (grocery-loc item) (hash-ref STORE-LOCATION item))
(define (aisle loc) (car loc))
(define (shelf loc) (cadr loc))
(define (aisle-end?) (memq (shelf location) (list MIN-SHELF MAX-SHELF)))

;; Actions
(define-action (right! n)              ; go right to aisle n
  #:prereq  (and (aisle-end?) (< (aisle location) n))
  location  (list n (shelf location) ))

(define-action (left! n)               ; go left to aisle n
  #:prereq  (and (aisle-end?) (> (aisle location) n))
  location  (list n (shelf location) ))

(define-action (up! n)                 ; go up to shelf n in current aisle
  #:prereq  (< (shelf location) n)
  location  (list (aisle location) n))

(define-action (down! n)               ; go down to shelf n in current aisle
  #:prereq  (> (shelf location) n)
  location  (list (aisle location) n))

(define-action (pickup! item)          ; add item on grocery list to trolley
  #:prereq  (equal? location (grocery-loc item))
  grocery-list (remove item grocery-list)
  trolley-contents (cons item trolley-contents) )

;; to display the state of the grocery store
(define (show-store)
   (printf "The grocery list contains ~a.\n" grocery-list)
   (printf "The robot is located in aisle ~a at shelf ~a.\n" 
           (aisle location) (shelf location))
   (printf "The trolley contents are ~a.\n" trolley-contents) )
