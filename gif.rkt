#lang racket
(provide render
         (all-from-out "algos.rkt"))
(require "draw.rkt" "algos.rkt" 2htdp/image 2htdp/universe)


(define (render path data huller [seconds 10])
  (define frames (reverse (draw/timing data huller)))
  (define seconds/frame (/ seconds (length frames)))
  (big-bang frames
            [record? path]
            [on-tick rest seconds/frame]
            [stop-when (compose null? rest)]
            [to-draw (compose coerce first)]))

(define (coerce i)
  (rotate 0 i))
