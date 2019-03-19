;;;   This is the odd bar problem, with 12 bars and 3 weighings allowed.

(define bars 12)
(define allowed-weighs 3)

(include "bars-bat.scm")

;; partition the list of all bars into 4 groups: both, heavy, light, neither
(define (ppart odds)
  (let loop ((b 0) (both '()) (heavy '()) (light '()) (neither '()))
    (if (= b bars) (list both heavy light neither)
        (if (member (list b 'heavy) odds)
            (if (member (list b 'light) odds)
                (loop (+ b 1) (cons b both) heavy light neither)
                (loop (+ b 1) both (cons b heavy) light neither) )
            (if (member (list b 'light) odds)
                (loop (+ b 1) both heavy (cons b light) neither)
                (loop (+ b 1) both heavy light (cons b neither)) )))))

;; choose k bars for left part of weighing action then call rbars for right
(define (lbars k part arg rem)
  (if (null? (cdr part))
      (if (> k (length (car part))) '()
          (let ((left (append (take (car part) k) arg)))
            (rbars (length left) (cons (drop (car part) k) rem) '() left)))
      (for/append ((i (+ 1 (min k (length (car part))))))
        (lbars (- k i) (cdr part) (append (take (car part) i) arg)
               (cons (drop (car part) i) rem)))))

;; choose k bars for right part of weighing action then build the action
(define (rbars k part arg left)
  (if (null? (cdr part))
      (if (> k (length (car part))) '()
          (list (weigh! left (append (take (car part) k) arg))))
      (for/append ((i (+ 1 (min k (length (car part))))))
        (rbars (- k i) (cdr part) (append (take (car part) i) arg) 
               left))))

;; the weighing actions representatives, in terms of possible values
(define (weigh-acts)
  (let ((pp (ppart (possible-values (list odd-bar odd-weight)))))
    (for/append ((k (quotient bars 2))) (lbars (+ k 1) pp '() '()))))

(define (main)
  (ergo-gendo
     (:begin (:for-all i allowed-weighs (:for-some a (weigh-acts) (:act a)))
             (:for-some a all-say-acts (:act a)))))
