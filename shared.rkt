#lang typed/racket
(provide Point Huller)
(define-type Point Complex)
(define-type Huller
  (->* ((Listof Point)) (((Sequenceof Point) (Sequenceof Point) (Sequenceof Point) * -> Void))
       (Listof Point)))

