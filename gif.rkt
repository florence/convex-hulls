#lang racket
(provide render
         (all-from-out "algos.rkt"))
(require "draw.rkt" "algos.rkt" 2htdp/image 2htdp/universe)


(define (render path data huller)
  (define draw (draw/timing data huller))
  (big-bang (draw)
            [record? path]
            [on-tick
             (lambda (f)
               (let ([n (draw)])
                 ((if (eq? n f) stop-with values) n)))
             .1]
            ;; an ugly hack to coerse to big-bang image
            [to-draw (curry rotate 0)]))
