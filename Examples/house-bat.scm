;;;  This is a specification of a world involving four rooms,
;;;  three doors, two boxes, and a robot.

;; the static part of the world
(define robot 'rob)
(define all-doors '(door1 door2 door3))
(define all-rooms '(room1 room2 room3 room4))
(define all-objects (cons robot '(box1 box2)))
(define door-connections               ; room topology
   (hasheq 'door1 '(room1 room2) 'door2 '(room2 room4) 'door3 '(room2 room3) ))

;;  the dynamic part of the world: two fluents
(define-fluents  
   open-door-list  '(door2 door3)      ; a list of open doors
   location-table                      ; a hash-table of object locations
        (hasheq 'box1 'room1  'box2 'room4  'rob 'room1) )

;;  useful abbreviations
(define (connected? d rm) (memq rm (hash-ref door-connections d)))
(define (open? d) (memq d open-door-list))
(define (location o) (hash-ref location-table o))
(define (ok-door? d rm1 rm2)
   (and (open? d) (connected? d rm1) (connected? d rm2)))
(define (traversable? rm1 rm2)
   (and (not (eq? rm1 rm2)) (for/or ((d all-doors)) (ok-door? d rm1 rm2))))

;;  three actions
(define-action (open! d)               ; open a door
   #:prereq (and (not (open? d)) (connected? d (location robot)))
   open-door-list (cons d open-door-list))

(define-action (goto! rm)              ; go to a room
   #:prereq (traversable? (location robot) rm)
   location-table (hash-set location-table robot rm))

(define-action (push-box! b rm)        ; push a box into a room
   #:prereq (and (eq? (location robot) (location b))
                 (traversable? (location robot) rm))
   location-table (hash-set* location-table b rm robot rm))

;; to display the state of the world
(define (show-house) 
   (printf "The open doors are ~a.\n" open-door-list)
   (printf "Box1 is in ~a. Box2 is in ~a. Rob is in ~a.\n"
           (location 'box1) (location 'box2) (location 'rob)) )

