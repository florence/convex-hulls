#lang racket
(require "draw.rkt" "algos.rkt" 2htdp/image 2htdp/universe)


(define (render path data huller)
  (big-bang (reverse (draw/timing data huller))
            [record? path]
            [on-tick rest .5]
            [stop-when (compose null? rest)]
            [to-draw (compose coerce first)]))

(define (coerce i)
  (rotate 0 i))
