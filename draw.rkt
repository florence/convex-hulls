#lang racket
(provide draw/timing draw)
(require 2htdp/image)

;; (listof complex) (-> (listof complex) (listof complex)) -> image
(define (draw/timing points algo)
  (define-values (lhull time _ __) (time-apply algo (list points)))
  (define hull (first lhull))
  (displayln `(got a ,(length hull) hull from ,(length points) points in ,time ms))
  (draw points hull))

(define (draw points hull)
  (define base (call-with-values (thunk (get-bounds hull)) empty-scene))
  (draw-points points (draw-hull hull base)))

;; (listof complex) -> real real
(define (get-bounds points)
  (define start (first points))
  (define-values (w h)
    (for/fold ([w (real-part start)] [h (imag-part start)]) ([p (rest points)])
      (values (if (< w (real-part p)) (real-part p) w)
              (if (< h (imag-part p)) (imag-part p) h))))
  (values (max w h) (max w h)))


(define (draw-points points base)
  (for/fold ([i base]) ([p points])
    (place-image (circle 5 'solid 'blue)
                 (real-part p)
                 (imag-part p)
                 i)))

(define (draw-hull hull base)
  (if (null? hull)
      base
      (let ([hull (append hull (list (first hull)))])
        (let loop ([hull hull] [base base])
          (cond [(null? (rest hull)) base]
                [else
                 (define x1 (real-part (first hull)))
                 (define y1 (imag-part (first hull)))
                 (define x2 (real-part (second hull)))
                 (define y2 (imag-part (second hull)))
                 (loop (rest hull) (add-line base x1 y1 x2 y2 'red))])))))
