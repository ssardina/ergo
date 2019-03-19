;;; The Jealous husband problem:
;;;   Three couples on the left side of the river
;;;   A boat that can hold 1 or 2 people.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Static facts about men, women and sides of river

(define men '(m1 m2 m3))                      ; 3 men
(define women '(w1 w2 w3))                    ; 3 women
(define people (append men women))            ; 6 people
(define (husband w) (case w ((w1) 'm1) ((w2) 'm2) ((w3) 'm3)))
(define (oppside x) (if (eq? x 'left) 'right 'left))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Fluents and actions

;; two fluents: location of the boat and location of the 6 people
(define-fluents   
  boat        'left
  people-loc  (hasheq  'm1 'left  'm2 'left  'm3 'left 
                       'w1 'left  'w2 'left  'w3 'left))

;; a useful abbreviation
(define (loc p) (hash-ref people-loc p))

;; cross the river with one person in the boat
(define-action (cross-single! p1)
  #:prereq   (eq? boat (loc p1))
  boat       (oppside boat)
  people-loc (hash-set people-loc p1 (oppside boat)))

;; cross the river with two people in the boat
(define-action (cross-double! p1 p2)
  #:prereq   (and (eq? boat (loc p1)) (eq? boat (loc p2)))
  boat       (oppside boat)
  people-loc (let ((opp (oppside boat))) (hash-set* people-loc p1 opp p2 opp)))
