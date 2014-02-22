#lang typed/racket
(provide draw/timing draw)
(require "shared.rkt"
         (only-in typed/mred/mred Snip%)
         plot/typed)

(: debug : (Parameterof Any))
(define debug (make-parameter #f))

(define-type Plot (U Void (Instance Snip%)))

(: draw/timing : (Listof Point) Huller -> Plot)
(define (draw/timing points algo)
  (define-values (lhull time _ __) (time-apply algo (list points)))
  (define hull (first lhull))
  (displayln `(got a ,(length hull) hull from ,(length points) points in ,time ms))
  (when (debug)
    (displayln `(with hull ,hull))
    (displayln `(and points ,points)))
  (draw points hull))

(: draw : (Listof Point) (Listof Point) -> Plot)
(define (draw p hull)
  (: to : (Listof Point) -> (Listof (Vectorof Real)))
  (define (to ps) (map (λ ([x : Point]) (vector (real-part x) (imag-part x))) ps))
  (plot 
   (list (points (to p))
         (lines (to (append hull (list (first hull)))))))) 

(: random-data : (->* () (Natural) (Listof Point)))
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
