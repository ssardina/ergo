;;;   A basic action theory for the game Pousse.
;;;   Details on the game itself can be found here:
;;;      http://web.mit.edu/drscheme_v424/share/plt/doc/games/pousse.html

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   The action theory: 3 fluents and 1 action

;; game constants
(define N 4)                              ; the size of the board
(define L '(0 1 2 3))                     ; the rows / cols as a list
(define D '(t b l r))                     ; the directions to push
(define N* (- N 1))                       ; last row and column

(define-fluents
  board   (build-array (list N N) (lambda (i j) #f)) ; the game board
  player  'x                              ; the player who plays next
  boards  '())                            ; a list of previous boards

(define-action (push! dir k)              ; push in direction dir on row/col k
  board  (move (trans dir k))             ; get new board from current one
  player (if (eq? player 'x) 'o 'x)       ; the turns alternate 
  boards (cons board boards))             ; save current board

;;  Useful abbrevs for access to board
(define (occ i j) (array-ref board (list i j)))
(define (move l) (apply array-set* board l))

;; print the board row by row
(define (print-board)
  (define (printrc i j) (display (or (occ i j) "-")))
  (for ((i L)) (display "  ") (for ((j L)) (printrc i j)) (display "\n")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   How the board is transformed after a pushing move

;; get a list (l1 p1 ... ln pn) where li is array indices and pi is player
(define (trans dir k)
  (case dir ((t) (pushl player 0  k +1  0)) 
            ((b) (pushl player N* k -1  0))
            ((l) (pushl player k  0  0 +1))
            ((r) (pushl player k  N* 0 -1)) ))

(define (pushl pl i j di dj)
  (define edge? (or (and (= i 0) (= di -1)) (and (= i N*) (= di +1))
                    (and (= j 0) (= dj -1)) (and (= j N*) (= dj +1))))
  (cons (list i j) (cons pl (if (or edge? (not (occ i j))) '()
                                (pushl (occ i j) (+ i di) (+ j dj) di dj)))))
