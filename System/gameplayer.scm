;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  A general game player, loaded on demand by (ergo-play-game . args)
;;;  or by (ergo-generate-move . args)

;; play a full game aginst exogenous opponent
(define (ergo-play-game-fn winner acts show
            #:first? [first? #t] #:static [static (lambda () 0)] 
            #:depth [depth 30]   #:infinity [infinity 1])

  ;; go through entire game from s with player1 and player2 as move generators
  (define (game-player winner show player1 player2)
    (printf "The current state is as follows:\n") (show)
    (case (winner)
      ((1) (printf "The game is over and the winner is Player 1.\n"))
      ((-1) (printf "The game is over and the winner is Player 2.\n"))
      ((0) (printf "The game is over in a tie.\n"))
      (else (let ((a (player1)))
              (save-state-excursion
               (printf "Action ~a is chosen.\n" a)
               (change-state a)
               (game-player winner show player2 player1))))))

;; move generator that displays legal moves and uses read (exogenous)
  (define (read-move acts)
    (lambda () 
      (let* ((poss (for/only ((a acts)) (legal-action? a)))
             (len (length poss)))
        (printf "The following actions are now possible:\n")
        (for ((ind len) (a poss)) (printf "  ~a  ~a\n" ind a))
        (printf "Enter a number from 0 to ~a to choose one: " (- len 1))
        (flush-output)
        (let ((ch (read)))
          (if (and (>= ch 0) (< ch len)) (list-ref poss ch)
              (error 'GAME "Number ~a is an illegal move choice" ch))))))

  (printf "--------------   Playing an entire game   ------------------\n")
  (printf "Player ~a moves will be generated (endogenous).\n" (if first? 1 2))
  (printf "Player ~a moves must be entered (exogenous).\n" (if first? 2 1))
  (game-player winner show 
    (if first? (ergo-generate-move-fn winner acts #t
                  #:static static #:depth depth #:infinity infinity)
        (read-move acts))
    (if first? (read-move acts)
        (ergo-generate-move-fn winner acts #f
            #:static static #:depth depth #:infinity infinity))))

;; generate a single move using minimax
(define (ergo-generate-move-fn winner acts max?
            #:static [static (lambda () 0)] 
            #:depth [depth 30] #:infinity [inf 1])

  ;; return values a and v where a is in acts and v is backed up value
  (define (minimax winner static acts depth inf max?)
    (let loop ((max? max?) (d depth) (alpha (- (+ inf 1))) 
               (beta (+ inf 1)) (cnt values))
      (let ((win (winner)))
        (if win (cnt #f (* win inf))
            (if (= d 0) (cnt #f (static))
                (list-extreme max? alpha beta
                  (for/only ((a acts)) (legal-action? a)) cnt 
                  (lambda (a alpha beta)
                    (save-state-excursion
                     (change-state a)
                     (loop (not max?) (- d 1) alpha beta (lambda (c v) v))
                     ))))))))

  ;; call cntfn on extreme (max or min) value of valfn on list x
  (define (list-extreme max? alpha beta x cnt valfn)
    (let loop ((x x) (best #f) (a alpha) (b beta))
      (if (null? x) (cnt best (if max? a b))
          (let* ((c (car x)) (rest (cdr x)) (v (valfn c a b)))
            (cond
             ((and max? (>= v b)) (cnt best b))             ; beta cutoff
             ((and (not max?) (<= v a)) (cnt best a))       ; alpha cutoff
             ((and max? (> v a)) (loop rest c v b))         ; best <- c
             ((and (not max?) (< v b)) (loop rest c a v))   ; best <- c
             (else (loop rest best a b)))))))               ; best unchanged
    
  (lambda ()
    (let-values (((a v) (minimax winner static acts depth inf max?)))
    a)))
