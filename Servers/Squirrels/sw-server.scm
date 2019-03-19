;;;                A SQUIRREL WORLD SERVER IN RACKET
;;;
;;;                           February 2009
;;;
;;;  This software was developed by Hector Levesque as part of the Cognitive 
;;;  Robotics group at the University of Toronto.
;;;
;;;       Do not distribute without permission.
;;;       Include this notice in any copy made.
;;;
;;;       Copyright (c) 2009 by The University of Toronto,
;;;            Toronto, Ontario, Canada.
;;;
;;;                         All Rights Reserved
;;;
;;;  Permission to use, copy, and modify, this software and its
;;;  documentation for research purpose is hereby granted without fee,
;;;  provided that the above copyright notice appears in all copies and
;;;  that both the copyright notice and this permission notice appear in
;;;  supporting documentation, and that the name of The University of
;;;  Toronto not be used in advertising or publicity pertaining to
;;;  distribution of the software without specific, written prior
;;;  permission.  The University of Toronto makes no representations about
;;;  the suitability of this software for any purpose.  It is provided "as
;;;  is" without express or implied warranty.
;;;
;;;  THE UNIVERSITY OF TORONTO DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
;;;  SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND
;;;  FITNESS, IN NO EVENT SHALL THE UNIVERSITY OF TORONTO BE LIABLE FOR ANY
;;;  SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
;;;  RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
;;;  CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
;;;  CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;    See the README for details on the squirrel world and this software

(require htdp/world)
(require racket/gui)
(require racket/runtime-path)

(define-runtime-path source-dir "./")

;; global variables for TCP
(define tcp-port 8123)                               ; the TCP port used
(define tcp-listener (tcp-listen tcp-port 4 #t))     ; 4 connections at most
(printf "Listening for connections on port ~a\n" tcp-port)

;; simulation constants
(define num-of-acorns 24)         ; how many randomly positioned acorns
(define num-of-walls 4)           ; how many randomly positioned walls
(define max-time 100000)          ; when should the simulation stop
(define game-goal-acorns 4)       ; how many acorns to win
(define max-holdings 2)           ; how many acorns can a squirrel carry
(define listen-range 6)           ; how far away can a squirrel hear others
(define max-energy 3000)          ; the max energy a squirrel can have 
(define acorn-energy 2700)        ; the energy gained by eating one acorn
(define collision-penalty 750)    ; how much energy is lost by hitting a wall

;; action durations
(define dur-left 1)               ; turning left
(define dur-right 1)              ; turning right
(define dur-forward 1)            ; going one step forward
(define dur-pick 30)              ; picking up an acorn
(define dur-drop 30)              ; dropping an acorn
(define dur-eat 60)               ; eating an acorn
(define dur-build 40)             ; building a wall segment
(define dur-feel 1)               ; sensing energy level
(define dur-smell 4)              ; sensing for an acorn or a squirrel
(define dur-listen 40)            ; sensing for more distant squirrels
(define dur-look 10)              ; sensing for a wall ahead

;; graphic constants
(define sim-speed 80)                             ; clock frequency measure
(define grid-size 20)                             ; pixels per grid square
(define cols 50)                                  ; num of grid cols
(define rows 30)                                  ; num of grid rows
(define wall-width 2)                             ; thickness of walls

;; graphic constants calculated via grid-size
(define half-grid (floor (/ grid-size 2)))
(define width (* grid-size cols))                 ; window width
(define height (* grid-size rows))                ; window height
(define text-font-size (floor (/ grid-size 2)))   ; for time and acorn text
(define squirrel-size (+ grid-size (floor (/ grid-size 4))))
(define acorn-size (floor (/ grid-size 3)))
(define acorn-pic (circle acorn-size 'solid 'brown))  
(define acorn-text-xoffset (floor (/ acorn-size -2)))
(define acorn-text-yoffset (- acorn-size))

;; this procedure is used to scale a bitmap in a file for a squirrel icon
(define (make-squirrel-pic str)
  (define file (string-append (path->string source-dir) str))
  (define orig-bitmap (make-object bitmap% file))
  (define w (send orig-bitmap get-width))
  (define h (send orig-bitmap get-height))
  (define new-bitmap (make-object bitmap% squirrel-size squirrel-size))
  (define bdc (make-object bitmap-dc% new-bitmap))
  (send bdc clear)
  (send bdc set-scale (/ squirrel-size w) (/ squirrel-size h))
  (send bdc draw-bitmap orig-bitmap 0 0)
  (send bdc set-bitmap #f)
  (make-object image-snip% new-bitmap))

;; 4 squirrels + expired squirrel in each of 4 orientations
(define nnpic (make-squirrel-pic "figs/sqNn.xpm"))
(define nspic (make-squirrel-pic "figs/sqNs.xpm"))
(define nepic (make-squirrel-pic "figs/sqNe.xpm"))
(define nwpic (make-squirrel-pic "figs/sqNw.xpm"))

(define snpic (make-squirrel-pic "figs/sqSn.xpm"))
(define sspic (make-squirrel-pic "figs/sqSs.xpm"))
(define sepic (make-squirrel-pic "figs/sqSe.xpm"))
(define swpic (make-squirrel-pic "figs/sqSw.xpm"))

(define enpic (make-squirrel-pic "figs/sqEn.xpm"))
(define espic (make-squirrel-pic "figs/sqEs.xpm"))
(define eepic (make-squirrel-pic "figs/sqEe.xpm"))
(define ewpic (make-squirrel-pic "figs/sqEw.xpm"))

(define wnpic (make-squirrel-pic "figs/sqWn.xpm"))
(define wspic (make-squirrel-pic "figs/sqWs.xpm"))
(define wepic (make-squirrel-pic "figs/sqWe.xpm"))
(define wwpic (make-squirrel-pic "figs/sqWw.xpm"))

(define xnpic (make-squirrel-pic "figs/sqXn.xpm"))
(define xspic (make-squirrel-pic "figs/sqXs.xpm"))
(define xepic (make-squirrel-pic "figs/sqXe.xpm"))
(define xwpic (make-squirrel-pic "figs/sqXw.xpm"))
(define xpics (list xnpic xspic xepic xwpic))

;;; Structures: acorns, walls, and squirrels

;; Acorn is not visible? if it is being carried or has been eaten
(define-struct acorn (visible? xpos ypos) #:mutable)

;; End points of walls.  Note: Wall will either be horizontal or vertical
(define-struct wall (x0 y0 x1 y1))                     ; not #:mutable

;; The squirrel structure has the following fields:
;;    name:  a string used for id
;;    xpos, ypos:  position on the grid where (0,0) is top left
;;    pics: images of the squirrel in 4 orientations (n,s,e,w)
;;    dir: current orientation, n, s, e, or w
;;    expire-time:  when the squirrel will expire (unless it eats)
;;    carrying: acorns being carried (in the cheeks?)
;;    cust: custodian of thread that interacts with squirrel agent (or #f)
;;    next-time: time when squirrel will be ready to act in the world
;;    act-ready?: has the next act for the squirrel been read over TCP?
;;    channel: channel used by thread to interact with main program
(define-struct squirrel
  (name xpos ypos pics dir expire-time carrying cust 
   next-time act-ready? comm-chan) 
  #:mutable)

;; miscellaneous utilities

;; number of elements in list l for which fn is true
(define (list-count fn l)
  (let loop ((l l) (acc 0))
    (if (null? l) acc (loop (cdr l) (if (fn (car l)) (1+ acc) acc)))))

;; all true applications of fn to elements of l
(define (sublist fn l)
  (let loop ((l l) (acc '()))
    (if (null? l) acc 
        (loop (cdr l) (let ((first (fn (car l)))) 
                        (if (not first) acc (cons first acc)))))))

;; like MIT scheme
(define (1+ x) (add1 x))
(define (-1+ x) (sub1 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Initialization 

;; for the random number generator
(define world-seed 0)
(define (new-seed) (random 2147483647 (make-pseudo-random-generator)))

;; global variables, may be re-assigned during the game
(define user-stopped? #f)        ; has the user requested a stop?
(define game-winner? #f)         ; is there a winner?

;; world global variables, assigned once by initialize-world below
(define acorn-list '())          ; the acorns
(define wall-list '())           ; the walls 
(define squirrel-list '())       ; the squirrels
(define background '())          ; the unchanging part of the scene

(define (initialize-world) 

  ;; generate randomly located acorns
  (define (ini-acorns)
    (for/list ((i (in-range num-of-acorns)))
              (make-acorn #t (random cols) (random rows))))

  ;; generate some random walls
  (define (ini-walls)
    (define (new-horiz-wall) 
      (let* ((y (1+ (random rows)))
             (l (1+ (random (-1+ cols))))
             (x (random (+ cols 2 (- l)))))
        (make-wall x y (+ x l) y)))
    (define (new-vert-wall) 
      (let* ((x (1+ (random cols)))
             (l (1+ (random (-1+ rows))))
             (y (random (+ rows 2 (- l)))))
        (make-wall x y x (+ y l))))
    (for/list ((i (in-range num-of-walls)))
              (if (= (random 2) 0) (new-vert-wall) (new-horiz-wall))))
  
  ;; always start with 4 squirrels (though some may be inactive)
  (define (ini-squirrels)
    (define sq1 
      (make-squirrel 'Nutty
         0 (- rows 1) (list nnpic nspic nepic nwpic)
             'n max-energy '() (make-custodian) 0 #f (make-channel)))
    (define sq2 
      (make-squirrel 'Skwirl 
             (- cols 1) 0 (list snpic sspic sepic swpic)
             's  max-energy '() (make-custodian) 0 #f (make-channel)))
    (define sq3 
      (make-squirrel 'Edgy 
             0 0 (list enpic espic eepic ewpic)
             'e max-energy '() (make-custodian) 0 #f (make-channel)))
    (define sq4
      (make-squirrel 'Wally 
             (- cols 1) (- rows 1) (list wnpic wspic wepic wwpic)
             'w max-energy '() (make-custodian) 0 #f (make-channel)))
    (list sq1 sq2 sq3 sq4))
  
  ;; the unchanging background scene
  (define (ini-scene)
    (add-walls wall-list (draw-grid-lines (empty-scene width height))))
  
  ;; assign the 4 world global variables
  (set! acorn-list (ini-acorns))
  (set! wall-list (ini-walls)) 
  (set! squirrel-list (ini-squirrels)) 
  (set! background (ini-scene))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Squirrel actions that change the world

;;; Squirrel expires

(define (act-quit sq time) (set-squirrel-expire-time! sq time) 'ok)

;;; Squirrel turning: always successful

(define (act-left sq time)
  (let ((dir (squirrel-dir sq)))
    (set-squirrel-dir! sq (case dir ((n) 'w) ((w) 's) ((s) 'e) (else 'n)))
    'ok))

(define (act-right sq time)
  (let ((dir (squirrel-dir sq)))
    (set-squirrel-dir! sq (case dir ((n) 'e) ((e) 's) ((s) 'w) (else 'n)))
    'ok))

;;; Squirrel moving forward 

(define (act-forward sq time)
  (if (blocked-passage? sq)  'fail
      (let ((xpos (squirrel-xpos sq)) (ypos (squirrel-ypos sq)))
        (case (squirrel-dir sq)
          ((n) (set-squirrel-ypos! sq (-1+ ypos)))
          ((e) (set-squirrel-xpos! sq (1+ xpos)))
          ((w) (set-squirrel-xpos! sq (-1+ xpos)))
          ((s) (set-squirrel-ypos! sq (1+ ypos))))
        'ok)))

;;; Is a squirrel unable to move forward?

(define (blocked-passage? sq)
  (let ((xpos (squirrel-xpos sq)) (ypos (squirrel-ypos sq)))
    (case (squirrel-dir sq)
      ((n) (blocked-north-passage? xpos ypos))
      ((s) (blocked-south-passage? xpos ypos))
      ((e) (blocked-east-passage? xpos ypos))
      ((w) (blocked-west-passage? xpos ypos)))))

(define (blocked-north-passage? x y)
  (or (= y 0) (for/or ((w wall-list))
                  (and (<= (wall-x0 w) x) 
                       (> (wall-x1 w) x) 
                       (= (wall-y0 w) y)))))

(define (blocked-south-passage? x y)
  (or (= y (-1+ rows)) (for/or ((w wall-list))
                           (and (<= (wall-x0 w) x) 
                                (> (wall-x1 w) x) 
                                (= (wall-y0 w) (+ 1 y))))))

(define (blocked-east-passage? x y)
  (or (= x (-1+ cols)) (for/or ((w wall-list))
                           (and (<= (wall-y0 w) y) 
                                (> (wall-y1 w) y) 
                                (= (wall-x0 w) (+ 1 x))))))

(define (blocked-west-passage? x y)
  (or (= x 0) (for/or ((w wall-list))
                  (and (<= (wall-y0 w) y) 
                       (> (wall-y1 w) y) 
                       (= (wall-x0 w) x)))))
          
;;; Squirrel actions that deal with acorns

(define (act-pick sq time)
  (let ((holding (squirrel-carrying sq))
        (xpos (squirrel-xpos sq)) 
        (ypos (squirrel-ypos sq)))
    (if (or (= (length holding) max-holdings)
            (> (num-squirrels-at xpos ypos) 1)) 'fail
      (let ((find (for/or ((ac acorn-list)) 
                     (and (acorn-at? ac xpos ypos) ac))))
        (if (not find) 'fail
            (begin 
              (set-acorn-visible?! find #f)
              (set-squirrel-carrying! sq (cons find holding))
              'ok))))))

(define (act-drop sq time)
  (let ((holding (squirrel-carrying sq)))
    (if (null? holding) 'fail
        (let ((xpos (squirrel-xpos sq)) (ypos (squirrel-ypos sq))
              (drop (car holding)))
          (set-acorn-visible?! drop #t)
          (set-acorn-xpos! drop xpos)
          (set-acorn-ypos! drop ypos)
          (set-squirrel-carrying! sq (cdr holding))
          (when (= (num-acorns-at xpos ypos) game-goal-acorns) 
                (printf "*** Squirrel ~a wins!\n" (squirrel-name sq))
                (for ((i 3)) (sleep .25) (bell))
                (set! game-winner? #t))
          'ok))))

(define (act-eat sq time)
   (let ((holding (squirrel-carrying sq)))
     (if (null? holding) 'fail
         (begin
           (set-squirrel-expire-time! sq 
                (min (+ acorn-energy (squirrel-expire-time sq))
                     (+ max-energy time)))
           (set-squirrel-carrying! sq (cdr holding))
           'ok))))

;;;  Squirrel building a new wall segment

(define (act-build sq time)
  (if (blocked-passage? sq) 'fail
      (let ((xpos (squirrel-xpos sq)) (ypos (squirrel-ypos sq)))
        (define new-wall-segment
           (case (squirrel-dir sq)
             ((e) (make-wall (1+ xpos) ypos (1+ xpos) (1+ ypos)))
             ((w) (make-wall xpos ypos xpos (1+ ypos)))
             ((s) (make-wall xpos (1+ ypos) (1+ xpos) (1+ ypos)))
             ((n) (make-wall xpos ypos (1+ xpos) ypos))))
        (set! wall-list (cons new-wall-segment wall-list))
        (set! background (add-walls (list new-wall-segment) background))
        'ok)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Squirrel actions that only sense the world

;; the default action when nothing else matches: return an info list
(define (act-help sq time)
  '(Legal actions are quit left right forward pick drop east build help
    look feel smell listen))

;; return 'wall or 'nothing
(define (act-look sq time)
  (if (blocked-passage? sq) 'wall 'nothing))

;; energy of the squirrel as a number from 0 (no energy) to 100 (max-energy)
(define (act-feel sq time)
  (round (/ (* 100 (max (- (squirrel-expire-time sq) time) 0)) max-energy)))

;; return list (i j) where i is # acorns and j is # other squirrels here
(define (act-smell sq time)
  (let ((xpos (squirrel-xpos sq)) (ypos (squirrel-ypos sq)))
    (list (num-acorns-at xpos ypos) (-1+ (num-squirrels-at xpos ypos)))))

(define (num-acorns-at x y)
  (list-count (lambda (ac) (acorn-at? ac x y)) acorn-list))

(define (acorn-at? ac x y) 
  (and (acorn-visible? ac) (= (acorn-xpos ac) x) (= (acorn-ypos ac) y)))

(define (num-squirrels-at x y)
  (list-count (lambda (sq) (squirrel-at? sq x y)) squirrel-list))

(define (squirrel-at? sq x y) 
  (and (squirrel-alive? sq) (= (squirrel-xpos sq) x) (= (squirrel-ypos sq) y)))

;; return a list of all pairs (i,j) where -d <= i,j <= d
;;      where d is the listen-range
;;   and another squirrel is located at relative position (i,j)
;;   with no intervening walls that would block the sound.
;; In rel cartesian coords, +i means to the left and +j means ahead 

(define (act-listen sq1 time)
  (sublist
   (lambda (sq2)
     (and (not (eq? sq1 sq2))
          (clear-squirrel-path? sq1 sq2)
          (let-values (([dx dy] (relative-delta sq1 sq2)))
                (and (<= (abs dx) listen-range) (<= (abs dy) listen-range)
                     (list dx dy)))))
   squirrel-list))

(define (relative-delta sq1 sq2)
  (let ((dx (- (squirrel-xpos sq2) (squirrel-xpos sq1)))
        (dy (- (squirrel-ypos sq2) (squirrel-ypos sq1))))
    (case (squirrel-dir sq1)
      ((s) (values (- dx) dy))
      ((n) (values dx (- dy)))
      ((e) (values dy dx))
      ((w) (values (- dy) (- dx))))))

;;; is the most direct path from sq1 to sq2 without intervening walls?

(define (clear-squirrel-path? sq1 sq2)

  (define (clear-path? x0 y0 x1 y1)
    (if (< (abs (- x1 x0)) 2)
        (clear-vertical? x0 y0 x1 y1)
        (if (< (abs (- y1 y0)) 2)
            (clear-horizontal? x0 y0 x1 y1)
            (let ((mx (round (/ (+ x1 x0) 2))) 
                  (my (round (/ (+ y1 y0) 2))))
              (and (clear-path? x0 y0 mx my)
                   (clear-path? mx my x1 y1))))))
  
  (define (clear-vertical? x0 y0 x1 y1)
    (case (- x1 x0)
      ((1) (and (not (blocked-east-passage? x0 y0))
                (clear-vertical? (1+ x0) y0 x1 y1)))    
      ((-1) (and (not (blocked-west-passage? x0 y0))
                 (clear-vertical? (-1+ x0) y0 x1 y1)))
      (else (or (= y0 y1)
                (if (< y0 y1) (and (not (blocked-south-passage? x0 y0))
                                   (clear-vertical? x0 (1+ y0) x1 y1))
                    (and (not (blocked-north-passage? x0 y0))
                         (clear-vertical? x0 (-1+ y0) x1 y1)))))))
  
  (define (clear-horizontal? x0 y0 x1 y1)
    (case (- y1 y0)
      ((1) (and (not (blocked-south-passage? x0 y0))
                (clear-horizontal? x0 (1+ y0) x1 y1)))
      ((-1) (and (not (blocked-north-passage? x0 y0))
                 (clear-horizontal? x0 (-1+ y0) x1 y1)))
      (else (or (= x0 x1)
                (if (< x0 x1) (and (not (blocked-east-passage? x0 y0))
                                   (clear-horizontal? (1+ x0) y0 x1 y1))
                    (and (not (blocked-west-passage? x0 y0))
                         (clear-horizontal? (-1+ x0) y0 x1 y1)))))))
  
  (clear-path? (squirrel-xpos sq1) (squirrel-ypos sq1) 
               (squirrel-xpos sq2) (squirrel-ypos sq2))
)

;;; The mapping from action symbols to durations and procedures

(define act-duration-table (make-immutable-hasheq (list 
  (cons 'left dur-left) (cons 'right dur-right) (cons 'forward dur-forward) 
  (cons 'pick dur-pick) (cons 'drop dur-drop) 
  (cons 'eat dur-eat) (cons 'build dur-build) 
  (cons 'look dur-look)  (cons 'feel dur-feel) 
  (cons 'smell dur-smell) (cons 'listen dur-listen))))

(define act-procedure-table (make-immutable-hasheq (list 
  (cons 'left act-left) (cons 'right act-right) (cons 'forward act-forward) 
  (cons 'pick act-pick) (cons 'drop act-drop) 
  (cons 'eat act-eat) (cons 'build act-build) 
  (cons 'look act-look)  (cons 'feel act-feel) (cons 'quit act-quit)
  (cons 'smell act-smell) (cons 'listen act-listen))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Threads are created to interact with each squirrel agent over TCP

;; start all the squirrel threads
(define (start-squirrels) 
  (for-each 
   (lambda (sq) 
     (parameterize ((current-custodian (squirrel-cust sq)))
        (thread (squirrel-process sq))))
   squirrel-list))

;; is a squirrel alive?  Yes, when the squirrel custodian is not #f
(define (squirrel-alive? sq) (squirrel-cust sq))

;; kill the squirrel thread and close ports using the squirrel's custodian
(define (stop-squirrel sq)
  (when (squirrel-alive? sq)
     (let ((cust (squirrel-cust sq)))
       (printf "Squirrel ~a expires\n" (squirrel-name sq))
       (set-squirrel-cust! sq #f)
       (custodian-shutdown-all cust))))

;; A thread of squirrel-process below is run for each connecting squirrel.
;; It reads the desired act over TCP and writes back the result.
;; It interacts with tick-handler over the comm-chan of the squirrel.
(define (squirrel-process me)
  (lambda ()
    (let-values ([(iport oport) (tcp-accept tcp-listener)])
       (printf "Squirrel ~a starts\n" (squirrel-name me))
       (network-write-result me (squirrel-name me) oport)
       (let ((chan (squirrel-comm-chan me))) 
         (let loop ((act (network-read-act me iport))) ; read act via TCP
           (set-squirrel-act-ready?! me #t)
           (channel-put chan act)                      ; send to tick handler
           (let* ((info (channel-get chan))            ; get from tick handler
                  (result (car info))                  ;  - the action result
                  (delay (cadr info)))                 ;  - the action delay
             (sleep (/ (-1+ delay) sim-speed))         ; wait before responding
             (network-write-result me result oport)    ; write result via TCP
             (loop (network-read-act me iport))))))))  ; repeat

;; read an act over TCP.  If it fails, the squirrel expires
(define (network-read-act sq iport) 
  (call-with-exception-handler (lambda (any) (stop-squirrel sq))
     (lambda () (read iport))))

;; write a result over TCP.  If it fails, the squirrel expires
(define (network-write-result sq result oport) 
  (call-with-exception-handler (lambda (any) (stop-squirrel sq))
     (lambda () (write result oport) (newline oport) (flush-output oport))))

;; Called each time the simulation clock ticks (see "simulate" below).
;; It interacts with each squirrel thread and then updates the time.
(define (tick-handler time)
  (for-each 
   (lambda (sq) 
     (if (< (squirrel-expire-time sq) time) (stop-squirrel sq)
        (when (and (squirrel-act-ready? sq) (>= time (squirrel-next-time sq)))
           (let* ((chan (squirrel-comm-chan sq))         
                  (act (channel-get chan))             ; get from sq thread
                  (dur (+ (act-duration act) (act-penalty sq act)))
                  (res (act-xeq-result sq act time)))  ; xeq the requested act
             (set-squirrel-next-time! sq (+ time dur))
             (set-squirrel-act-ready?! sq #f)
             (channel-put chan (list res dur))))))     ; send to sq thread
     squirrel-list)
   (1+ time))                                          ; update time

;; How long does an act take?  Look it up in the duration table (default=1).
(define (act-duration act) (hash-ref act-duration-table act 1))

;; The penalty for doing an act is 0 unless act=forward with blocked-passage
(define (act-penalty sq act)
  (if (and (eq? act 'forward) (blocked-passage? sq)) collision-penalty 0))

;; Look for an action in the act-procedure-table (default=act-help)
;; and then run it for the given squirrel and time, returning the result.
(define (act-xeq-result sq act time) 
  ((hash-ref act-procedure-table act (lambda () act-help))
   sq time))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Drawing routines for the simulation window

(define (draw-grid-lines sc)
  (let* ((half half-grid) (endr (- height half)) (endc (- width half)))
    (let loopr ((sc sc) (r half))
      (if (> r endr)
          (let loopc ((sc sc) (c half))
            (if (> c endc) sc
                (loopc (scene+line sc c half c endr 'tan) (+ c grid-size))))
          (loopr (scene+line sc half r endc r 'tan) (+ r grid-size))))))

(define (add-acorns acs sc)
  (if (null? acs) sc
      (if (acorn-visible? (car acs)) 
          (place-image 
         (overlay/xy acorn-pic acorn-text-xoffset acorn-text-yoffset
        (acorn-text (car acs)))
             (+ (* (acorn-xpos (car acs)) grid-size) half-grid)
             (+ (* (acorn-ypos (car acs)) grid-size) half-grid)
             (add-acorns (cdr acs) sc))
          (add-acorns (cdr acs) sc))))

(define (acorn-text ac)
  (let ((x (acorn-xpos ac)) (y (acorn-ypos ac)))
    (text (number->string (num-acorns-at x y)) text-font-size 'white)))

(define (add-walls walls sc)
  (if (null? walls) sc
      (let* ((w (car walls))
             (x0 (* (wall-x0 w) grid-size)) (y0 (* (wall-y0 w) grid-size))
             (x1 (* (wall-x1 w) grid-size)) (y1 (* (wall-y1 w) grid-size))
             (dx (+ wall-width (- x1 x0))) (dy (+ wall-width (- y1 y0))))
        (place-image 
           (move-pinhole (rectangle dx dy 'solid 'black) (/ dx -2) (/ dy -2))
           x0 y0 (add-walls (cdr walls) sc)))))

(define (add-squirrels sqs sc)
  (if (null? sqs) sc
      (place-image (squirrel-image (car sqs))
     (+ (* (squirrel-xpos (car sqs)) grid-size) half-grid)
     (+ (* (squirrel-ypos (car sqs)) grid-size) half-grid)
     (add-squirrels (cdr sqs) sc))))

;; use the saved squirrel images or xpics when it has expired 
(define (squirrel-image sq)
  (let ((pics (if (squirrel-alive? sq) (squirrel-pics sq) xpics)))
    (case (squirrel-dir sq) 
       ((n) (car pics)) ((s) (cadr pics)) 
       ((e) (caddr pics)) ((w) (cadddr pics)))))

(define (add-clock-text time sc)
  (place-image (text (number->string time) text-font-size 'black)
               (* 1.5 grid-size) (- height (+ text-font-size 2)) sc))

;; this is called whenever the screen needs to be refreshed
(define (redraw-scene time)
    (add-squirrels squirrel-list 
        (add-acorns acorn-list 
            (add-clock-text time background))))

;; this looks for the 'q' keystroke in the simulation window
(define (key-dispatch time k)
  (when (eq? k #\q) (kill-everything-and-exit)) time)

;; this alternate key-dispatch allows squirrel Nutty to be controlled 
;; manually, which can be useful for debugging (use "on-key-event" below)
(define (alt-key-dispatch time k)
  (let ((sq (car squirrel-list)))  ; sq = first squirrel = Nutty
    (case k
      ((#\q) (kill-everything-and-exit))  ; as before
      ((left) (act-left sq time))         ; arrow key
      ((right) (act-right sq time))       ; arrow key
      ((up) (act-forward sq time))        ; arrow key
      ((#\p) (act-pick sq time))
      ((#\d) (act-drop sq time))
      ((#\e) (act-eat sq time))
      ((#\b) (act-build sq time))
      ((#\f) (printf "Feel: ~a\n" (act-feel sq time)))
      ((#\l) (printf "Look: ~a\n" (act-look sq time)))
      ((#\s) (printf "Smell: ~a\n" (act-smell sq time)))
      ((#\i) (printf "Listen: ~a\n" (act-listen sq time))) )
    time))

;; shut down everything and exit
(define (kill-everything-and-exit)
  (display "Network connection going down\n")
  (custodian-shutdown-all (current-custodian))
  (exit 0))

;; this determines when to stop the simulation
(define (game-over time) 
  (and (or user-stopped? (> time max-time) game-winner?)
       (begin 
     (sleep 5)
     (kill-everything-and-exit))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Main routine

(define (simulate)
    (random-seed world-seed)
    (initialize-world)
    (start-squirrels)
    ; these are the procedures provided by world.ss for graphical simulation
    ; Note: the mapping from sim-speed to elapsed time is far from exact!
    (big-bang (+ width 10) (+ height 10) (/ 1 sim-speed) 0)
    (on-tick-event tick-handler)
    (on-redraw redraw-scene)
    (on-key-event key-dispatch)  ; or use alt-key-dispatch
    (stop-when game-over))

;; To start the server, at the command-line:  racket -tm <thisfile> [ <seed> ]
;; In an interactive window:  (main) or (main "<seed>")
(define (main . strs)
  (set! world-seed (if (null? strs) (new-seed) (string->number (car strs))))
  (simulate)
  (printf "Seed for this run was ~a\n" world-seed))  ; for replaying
