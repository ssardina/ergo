;;;  This programs simulates the operation of an elevator using gracket.
;;;  It expects to read a list of actions to perform of the form
;;;     (up n)       go up to floor n
;;;     (down n)     go down to floor n
;;;     (turnoff n)  turnoff the call button on floor n
;;;  A list of actions of this type can be piped to this server program
;;;  by the Ergo elevator controller Examples/BasicElevator/elevator-main.scm:
;;;      racket -l ergo -fm <controller> | gracket -fm <this program>

(require htdp/world)                                ; for simulation 
(require scheme/gui)                                ; for images
(require racket/runtime-path)                       ; for finding image files

(define-runtime-path elevbits "ele.png")            ; path to elevator image
(define-runtime-path buttbits "red.jpg")            ; path to button image 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Global constants

;; elevator world variables  
(define topfloor 24)                               ; top floor of building
(define button-vector (make-vector topfloor #f))   ; state of call buttons
(define elevator 0)                                ; pos of elevator in pixels
(define action-list '())                           ; the actions to xeq 

;; graphic constants
(define sim-speed 50)                              ; clock frequency measure
(define grid-size 30)                              ; pixels per grid square
(define action-pause 0)                            ; time until next action
(define action-dur 20)                             ; time between actions

;; graphic constants calculated via grid-size
(define width (* grid-size 3))                     ; window width
(define height (* grid-size (+ topfloor 1)))       ; window height
(define half-grid (floor (/ grid-size 2)))
(define font-size (- (floor (/ grid-size 2)) 2)) 
(define elevator-size (+ grid-size (floor (/ grid-size 4))))
(define button-size half-grid)
(define (current-floor) (1+ (- topfloor (/ elevator grid-size))))

;; this utility procedure scales a picture in a file to make a screen icon
(define (make-pic size file)
  (define orig-bitmap (make-object bitmap% file))
  (define w (send orig-bitmap get-width))
  (define h (send orig-bitmap get-height))
  (define new-bitmap (make-object bitmap% size size))
  (define bdc (make-object bitmap-dc% new-bitmap))
  (send bdc clear)
  (send bdc set-scale (/ size w) (/ size h))
  (send bdc draw-bitmap orig-bitmap 0 0)
  (send bdc set-bitmap #f)
  (make-object image-snip% new-bitmap))

;; bitmaps for elevator and button on (change path as needed)
(define elevpic (make-pic elevator-size elevbits))
(define onpic (make-pic button-size buttbits))
(define offpic (make-pic button-size "null"))

(define (1+ n) (+ n 1))
(define (-1+ n) (- n 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Drawing routines.  See htdp/image in reference manual

(define (draw-grid-lines sc)
  (let* ((half half-grid) (bnd (+ half (* 2 grid-size)))
     (endr (- height half)) (endc (- width half)))
    (let loopr ((sc sc) (r half))
      (if (> r endr) 
      (scene+line (scene+line sc bnd half bnd endr 'tan)  ; vertical L
              half half half endr 'tan)               ; vertical R
          (loopr (scene+line sc half r endc r 'tan) (+ r grid-size))))))

(define (draw-fixed-text sc)
  (let loop ((n topfloor))
    (if (= n 0) 
    (place-image (text "Type q to exit" (- font-size 2) 'black) 10 1 sc)
    (place-image (text (if (< n 10) (string-append " " (number->string n))
                   (number->string n))
               font-size 'blue) 
             0 (- (* (1+ (- topfloor n)) grid-size) (/ font-size 2))
             (loop (-1+ n))))))

(define (draw-call-buttons vec n sc)
  (if (= n 0) sc
      (place-image (if (vector-ref button-vector (-1+ n)) onpic offpic)
          grid-size (* (1+ (- topfloor n)) grid-size)
      (draw-call-buttons vec (-1+ n) sc))))

(define (draw-elevator ypos sc) (place-image elevpic (* 2 grid-size) ypos sc))

(define (draw-clock-text time sc)
  (place-image (text (number->string time) font-size 'darkgray)
               (* 1.5 grid-size) (- height (+ font-size 2)) sc))

;; the unchanging part of a scene
(define background               
  (draw-fixed-text (draw-grid-lines (empty-scene width height))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Simulation routines.  See htdp/world in reference manual

;; draw elevator, buttons, and clock time onto fixed background
(define (redraw-scene time)
  (draw-elevator elevator
      (draw-call-buttons button-vector topfloor
          (draw-clock-text time background))))

;; ignore all keys except "q" typed unto graphic window
(define (key-dispatch time k) (when (eq? k #\q) (exit 0)))

;; at each tick: wait or move elevator or turnoff current button
(define (tick-handler time) 
  (define (action-done)
    (set! action-pause action-dur)
    (set! action-list (cdr action-list)))
  (if (> action-pause 0) (set! action-pause (-1+ action-pause))
      (if (null? action-list) (exit 0)
     (let ((act (caar action-list)) (arg (cadar action-list)))     
       (case act
         ((up down)   
          (set! elevator (+ elevator (if (eq? act 'up) -1 1)))
          (when (and (= (modulo elevator grid-size) 0) 
             (= arg (current-floor)))
            (action-done)))
         ((turnoff) 
          (vector-set! button-vector (-1+ arg) #f)
          (action-done))
         ))))
  (1+ time))

(define (elevator-simulation)
  (big-bang (+ width 10) (+ height 10) (/ 1 sim-speed) 0)
  (on-tick-event tick-handler)
  (on-redraw redraw-scene)
  (on-key-event key-dispatch)
  (stop-when (lambda (time) #f)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  The main program

(define (main) 
  (define ini-floor 7)            ; the elevator starts on floor 7
  (define on-buttons '(3 5))      ; call buttons 3 and 5 are on
  (set! action-list (read))       ; obtain actions from standard input
  (set! elevator (* (1+ (- topfloor ini-floor)) grid-size))
  (for-each (lambda (x) (vector-set! button-vector (-1+ x) #t)) on-buttons)
  (elevator-simulation)           ; start the simulation
  (values))                       ; no output

