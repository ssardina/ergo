#lang racket

(define ergo-version "1.5")
(define ergo-date "Mar 2018")

;;;  This is the main file that is loaded when the -l ergo library is used

(eprintf "    Loading ERGO v~a (~a) in Racket v~a ...\n"
         ergo-version ergo-date (version))

(require "misc.scm")
(require "loader.scm")
(require "arrays.scm")
(require "random.scm")
(require "batter.scm")
(require "proglang.scm")
(require "interface.scm")
(require racket/runtime-path)

(define-runtime-path syspath "./")
(define (ergo-path str) 
  (let* ((prefix (path->string syspath)) (len (string-length prefix)))
    (string-append (substring prefix 0 (- len 2)) str)))

(provide (all-from-out racket))
(provide (all-from-out "misc.scm"))
(provide (all-from-out "loader.scm"))
(provide (all-from-out "arrays.scm"))
(provide (all-from-out "random.scm"))
(provide (all-from-out "batter.scm"))
(provide (all-from-out "proglang.scm"))
(provide (all-from-out "interface.scm"))

;; Things to do with BATs other than ergo-do
(define-on-demand ergo-simplan (ergo-path "simplan.scm"))
(define-on-demand ergo-genplan (ergo-path "genplan.scm"))
(define-on-demand ergo-play-game (ergo-path "gameplayer.scm"))
(define-on-demand ergo-generate-move (ergo-path "gameplayer.scm"))
(define-on-demand ergo-do (ergo-path "ergo-do.scm"))
(define-on-demand ergo-gendo (ergo-path "gendo.scm"))
(provide ergo-simplan ergo-genplan ergo-play-game ergo-generate-move
         ergo-do ergo-gendo)

;; change printer to pretty-printer
(current-print pretty-print-handler)

