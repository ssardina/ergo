;;;   The game of tic tac toe using a general game player.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   The action theory: 2 fluents and 1 action

(define-fluents 
   board  (vector #f #f #f #f #f #f #f #f #f #f)  ; the initial board 
   player 'x)                                 ; player who plays next

;; the current player occupies the board at position sq 
(define-action (move! sq) 
   #:prereq (not (vector-ref board sq))         ; square is not occupied
   board    (vector-set board sq player)        ; player occupies square
   player   (if (eq? player 'x) 'o 'x))         ; the turns alternate

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Auxiliary definitions

(define squares '(1 2 3 4 5 6 7 8 9)) 

(define lines  '((1 2 3) (4 5 6) (7 8 9) (1 4 7) (2 5 8) (3 6 9) 
                 (3 5 7) (1 5 9)))

(define (occ sq) (vector-ref board sq))

(define (has-line? pl)                       ; player pl owns some line?
  (for/or ((ln lines)) (for/and ((sq ln)) (eq? pl (occ sq)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   The game playing 

(define (winner)                             ; the winner of a board or #f
   (if (has-line? 'x) 1                                        ; player1
      (if (has-line? 'o) -1                                    ; player2
          (and (and-map occ squares) 0))))                     ; a tie

(define (print-board)                        ; display 3 lines of the board
   (define (printsq sq) (display (or (occ sq) "-")))
   (define (println ln) (display "   ") (for-each printsq ln) (display "\n"))
   (for-each println '((1 2 3) (4 5 6) (7 8 9))))

;; X moves first via minimax; O plays via read
(define (main) (ergo-play-game winner (map move! squares) print-board))
