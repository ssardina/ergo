;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   This is the odd bar problem, where the number of bars and the number
;;;      of allowed weigh actions are defined elsewhere

;;;  Fluents:
;;;    odd-bar:  the bar that is odd
;;;    odd-weight: heavy or light for the odd bar
;;;    announced?:  true only after odd bar has been announced
;;;    tries:  numbers of weighing actions remaining
;;;  Actions:
;;;    (say! b w): announce that bar b is the odd one and of weight w
;;;    (weigh! l1 l2): compare weight of the bars in list l1 vs list l2

(define weights '(heavy light))

(define-states ((b bars) (w weights))      ; bars defined elsewhere
  odd-bar    b
  odd-weight w
  announced? #f
  tries      allowed-weighs)               ; allowed-weighs defined elsewhere

(define-action (say! b w)
  #:prereq   (and (eq? b odd-bar) (eq? w odd-weight))
  announced? #t)

(define-action (weigh! l1 l2)
  #:prereq   (and (> tries 0) (= (length l1) (length l2)))
  #:sensing  (cond
               ((memq odd-bar l1) (if (eq? odd-weight 'heavy) 'left 'right))
               ((memq odd-bar l2) (if (eq? odd-weight 'heavy) 'right 'left))
               (else 'even))
  tries      (- tries 1))

(define all-say-acts
  (for/append ((b bars)) (for/list ((w weights)) (say! b w))))
