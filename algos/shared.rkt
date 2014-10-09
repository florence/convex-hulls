#lang typed/racket
(provide Point Huller FrameDrawer
         random-data random-point)
(require math/distributions)
(define-type Point Complex)
(define-type Huller
  (->* ((Listof Point)) (FrameDrawer)
       (Listof Point)))
(define-type FrameDrawer
  ((Sequenceof Point) (Sequenceof Point) (Sequenceof Point) * -> Void))


(: random-data : (->* () (Natural) (Listof Point)))
(define (random-data [n 100]) 
  (for/list ([_ n]) (random-point)))

(define BOUND 100)
(define d (normal-dist (/ BOUND 2) 25))
(: random-point : (-> Point))
(define (random-point)
  (define xs (sample d))
  (define ys (sample d))
  (make-rectangular xs ys))
