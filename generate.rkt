#!/usr/bin/env racket
#lang racket
(module+ main
  (require "gif.rkt" "algos/gift-wrap.rkt" "algos/shared.rkt"
           "algos/monotone.rkt" "algos/quickhull.rkt")

  (define file "/tmp/gif.rkt")
  (define v 
    (command-line 
     #:once-each
     [("-f" "--file") f "file to output to. defaults to /tmp/gif.gif"
      (set! file f)]
     #:args (type) (string-downcase type)))
  (match v
    [(or "g" "gift-wrap" "gift-wrap")
     (render file (random-data) gift-wrap)]
    [(or "m" "monotone")
     (render file (random-data) monotone 20)]
    [(or "q" "quickhull" "quick-hull")
     (render file (random-data 200) quickhull 20)]))
