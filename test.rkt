#lang typed/racket
(require "gif.rkt" "algos/gift-wrap.rkt" "algos/shared.rkt"
         "algos/monotone.rkt")
;(debug #t)
(render "/tmp/gif.gif" (random-data) #;gift-wrap
        monotone)
