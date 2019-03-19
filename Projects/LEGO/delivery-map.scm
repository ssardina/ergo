;;;  This is the code that defines the map of roads for the LEGO robot

;; The table below is for a map that looks like this:
;;     1 --- 2 --------- 7 ---8
;;          /            |     \ 
;;          |            |      \ 
;;  3 ----- 4 --- 5 ---- 6 ----- 9
;;         /             |                
;;       10             11
(define adjacency
  (hasheq 1 '(#f 2 #f #f)  2 '(#f 7 4 1)    3 '(#f 4 #f #f)  4 '(3 2 5 10)
          5 '(4 #f 6 #f)   6 '(5 7 9 11)    7 '(2 #f 8 6)    8 '(7 #f #f 9)
          9 '(6 8 #f #f)  10 '(#f 4 #f #f) 11 '(#f 6 #f #f)))

(define (adjs x) (hash-ref adjacency x #f))

;; the next location after leaving x with orientation ori
(define (next-location x ori) (list-ref (adjs x) ori))

;; the next orientation after leaving x with orientation ori
(define (next-orientation x ori)
  (let ((locs (adjs (next-location x ori))))
    (modulo (+ (for/or ((i 4)) (and (eq? x (list-ref locs i)) i)) 2) 4)))

;; the orientation that results from turning dir = left, right or around
(define (shift-orientation ori dir)
  (modulo (+ ori (case dir ((left) -1) ((right) 1) ((around) 2))) 4))

;; the dir that is needed to go from x and ori to an adjacent location y
(define (get-direction x ori y)
  (let ((locs (adjs x)))
    (case (for/or ((i 4)) (and (eq? y (list-ref locs (modulo (+ ori i) 4))) i))
      ((0) #f) ((1) 'right) ((2) 'around) ((3) 'left))))

;; a path of adj locations whose first element=start and last element=end 
(define (find-path start end)
  (let loop ((seen '()) (nodes (list (list start))))
    (define (nexts x) (for/only ((y (adjs x))) (and y (not (memq y seen)))))
    (define (next-paths x path) (for/list ((y (nexts x))) (cons y path)))
    (if (null? nodes) #f
        (let ((path (car nodes)) (x (caar nodes)))
          (if (eq? x end) (reverse path)
              (loop (cons x seen) (append (cdr nodes) (next-paths x path))))))))
