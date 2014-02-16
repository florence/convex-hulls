#lang racket
(provide draw/timing draw)
(require plot)

(define debug (make-parameter #f))

;; (listof complex) (-> (listof complex) (listof complex)) -> image
(define (draw/timing points algo)
  (define-values (lhull time _ __) (time-apply algo (list points)))
  (define hull (first lhull))
  (displayln `(got a ,(length hull) hull from ,(length points) points in ,time ms))
  (when (debug)
    (displayln `(with hull ,hull))
    (displayln `(and points ,points)))
  (draw points hull))

(define (draw p hull)
  (define (to ps) (map (λ (x) (vector (real-part x) (imag-part x))) ps))
  (plot 
   (list (points (to p))
         (lines (to (append hull (list (first hull)))))))) 

;; (listof complex) -> real real
(define (get-bounds points)
  (define start (first points))
  (define-values (w h)
    (for/fold ([w (real-part start)] [h (imag-part start)]) ([p (rest points)])
      (values (if (< w (real-part p)) (real-part p) w)
              (if (< h (imag-part p)) (imag-part p) h))))
  (values (max w h) (max w h)))


(define (random-data [n 100]) 
  (for/list ([_ n]) (make-rectangular (random 100) (random 100))))


(module+ gift-wrap
  (require "algos.rkt")
  (provide test)
  (define (test)
    (define data (random-data))
    (parameterize ([debug #t])
      (draw/timing data gift-wrap))))

;(define-syntax (run sub) (require (submod "." sub)))
