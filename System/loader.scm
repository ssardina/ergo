#lang racket

;;; This is an autoloader hack to avoid always loading all definitions.
;;;   (It allows specialized programs like game players, planners, etc.)
;;; A file is loaded the first time a define-on-demand function is called.

(require "misc.scm")
(provide load-on-demand-table load-on-demand define-on-demand)

;; The table used for on-demand loading.  It maps symbols to files. 
(define load-on-demand-table (make-hasheq))

;; load the file associated with fn symbol in table, then zap that entry.
(define (load-on-demand fn)
  (let ((file (hash-ref load-on-demand-table fn #f)))
    (when file 
          (eprintf "Loading on demand ~a\n" file)
          (load file)
          (hash-set! load-on-demand-table fn #f))))

;; define macro for (sym ...)  where file contains (define (sym-fn ...) ...)
(define-macro (define-on-demand sym file)
  (let* ((sym-fn (string->symbol (string-append (symbol->string sym) "-fn")))
         (first `(load-on-demand ',sym))
         (second (list sym-fn ',@args)))
    `(begin
       (hash-set! load-on-demand-table ',sym ,file)
       (define-macro (,sym . args)
         ,(list 'quasiquote (list 'begin first second))))))

