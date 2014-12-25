#!/usr/bin/env racket
#lang racket
(require "gif.rkt" "algos/gift-wrap.rkt" "algos/shared.rkt"
         "algos/monotone.rkt" "algos/quickhull.rkt")

(define v (string-downcase (command-line #:args (type) type)))
(match v
  [(or "g" "gift-wrap" "gift-wrap")
   (render "/tmp/gif.gif" (random-data) gift-wrap)]
  [(or "m" "monotone")
   (render "/tmp/gif.gif") (random-data) monotone]
  [(or "q" "quickhull" "quick-hull")
   (render "/tmp/gif.gif" (random-data 200) quickhull 20)])
