;;;   The game Pousse in Ergo using the general game player.

(include "pousse-bat.scm")

;; number of cols (resp rows) occupied by player pl in a row (resp col)
(define (num-cols pl i) (for/sum ((j L)) (if (eq? pl (occ i j)) 1 0)))
(define (num-rows pl j) (for/sum ((i L)) (if (eq? pl (occ i j)) 1 0)))

;; sum over all rows (resp cols) of fn of #cols (resp rows) for X and for O
(define (row-counter fn)
  (for/sum ((i L)) (fn (num-cols 'x i) (num-cols 'o i))))
(define (col-counter fn)
  (for/sum ((j L)) (fn (num-rows 'x j) (num-rows 'o j))))

(define (occupancy)                              ; occupancy squared
  (define (sq xs os) (- (* xs xs) (* os os)))
  (+ (row-counter sq) (col-counter sq)))

(define (centrality)                             ; manhattan dist to corners
  (for/sum ((i L))
    (for/sum ((j L))           
      (* (case (occ i j) ((x) 1) ((o) -1) (else 0))
         (+ (min i (- N i)) (min j (- N j)))))))

(define (static) (+ (occupancy) (centrality)))   ; static evaluation

(define (winner)                                 ; the winner of a board
  (define (owned xs os) (if (= xs N) 1 (if (= os N) -1 0)))
  (if (member board boards) (if (eq? player 'x) 1 -1)
      (let ((ownership (+ (row-counter owned) (col-counter owned))))
        (if (> ownership 0) 1 (if (< ownership 0) -1 #f)))))

(define all-moves (for/append ((dir D)) (for/list ((k L)) (push! dir k))))

;; X moves first via minimax; O plays via read
(define (main) (ergo-play-game winner all-moves print-board
                               #:static static #:depth 6 #:infinity 512))
