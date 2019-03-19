;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   This is the odd bar problem, with 3 bars and 2 weighings allowed.

(define bars 3)
(define allowed-weighs 2)

(include "bars-bat.scm")

(define (main)
  (ergo-genplan (lambda () announced?) 
    (append all-say-acts 
            '((weigh! (0) (1)) (weigh! (0) (2)) (weigh! (1) (2))) )))

