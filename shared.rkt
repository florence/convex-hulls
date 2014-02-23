#lang typed/racket
(provide Point Huller FrameDrawer)
(define-type Point Complex)
(define-type Huller
  (->* ((Listof Point)) (FrameDrawer)
       (Listof Point)))
(define-type FrameDrawer
  ((Sequenceof Point) (Sequenceof Point) (Sequenceof Point) * -> Void))

