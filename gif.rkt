#lang typed/racket
(provide render debug)
(require "draw.rkt" "algos/shared.rkt"
         typed/net/gifwrite)

(define DELAY 10);/100ths of a second

(: render : String (Listof Point) Huller -> Void)
;; A lot of this code is stolen from file/gif
(define (render path data huller)
  (define draw (draw/timing data huller))
  (call-with-output-file*
   path
   (lambda ([p : Output-Port])
     (define first (draw))
     (cond 
      [(not first)
       (error 'drawing "need at least one image")]
      [else
       (define count 0)
       (define w (send first get-width))
       (define h (send first get-height))
       (define stream (gif-start p w h 0 #f))
       (gif-add-loop-control stream 0)
       (let loop ([btmp : OPlot (draw)])
         (when btmp
           (when (debug)
             (set! count (add1 count))
             (display "draw" (current-error-port))
             (displayln count (current-error-port)))
           (define-values (pxls colormap transparent)
             (let ([argb (make-bytes (* w h 4) 255)]
                   [mask (send btmp get-loaded-mask)])
               (send btmp get-argb-pixels 0 0 w h argb)
               (when mask
                 (send mask get-argb-pixels 0 0 w h argb #t))
               (quantize argb)))
           (define next (draw))
           (gif-add-control stream 'any #f (if next DELAY (* DELAY 10)) transparent)
           (gif-add-image stream 0 0 w h #f colormap pxls)
           (loop next)))
       (when (debug)
         (displayln "end" (current-error-port)))
       (gif-end stream)]))))
