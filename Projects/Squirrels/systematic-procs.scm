;;  position or direction abbrevs
(define dirL '((north west) (west south) (south east) (east north)))
(define dirR '((north east) (west north) (south west) (east south)))
(define ymax (case sw-name ((Edgy Wally) 50) (else 30)))

;; do sensing action a and wait for exogenous response
(define (check a) (:atomic (:act a) (:wait)))

;; action procedures
(define (face-dir dir)       ; rotate to face dir
  (:if (eq? dir direction) :nil
       (:if (eq? dir (cadr (assq direction dirR))) (:act right)
            (:if (eq? dir (cadr (assq direction dirL))) (:act left)
                 (:begin (:act right) (:act right))))))

(define (go-dir dir)         ; face dir, then one step forward
  (:begin (face-dir dir) (:act forward)))

(define (dump-at-nest)       ; drop all carried acorns at nest
  (do-at-nest (:for-all i carrying (:act drop))))

(define (status-report)
  (:>> (printf "Energy=~a Carrying=~a Stashed=~a\n" energy carrying stashed)))

(define (acorns-present)     ; behaviour when acorns are smelled
  (:while (> smelled 0)   
     (:when (= carrying 2) (dump-at-nest))
     (:act pick)
     (:when (= 4 (+ carrying stashed)) (dump-at-nest))  ; I win!
     (check feel) (check smell)))

(define (low-energy)         ; behaviour when feeling hungry
  (:when (< energy 25)
     (:>> (printf "*** Low energy! ")) (status-report)
     (:when (= carrying 0)
        (:if (> stashed 0) (do-at-nest (:act pick)) (:act quit)))   ; I lose!
     (:act eat) (check feel)))
