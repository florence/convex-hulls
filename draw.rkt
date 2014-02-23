#lang typed/racket
(provide draw/timing)
(require "shared.rkt"
         (only-in typed/mred/mred Snip%)
         plot/typed
         plot/typed/utils)

(: debug : (Parameterof Any))
(define debug (make-parameter #f))

;; We never generate Voids, but we need TR to play nice
(define-type Plot (Instance Snip%))

(: draw/timing : (Listof Point) Huller -> (Listof Plot))
(define (draw/timing points algo)
  (: frames : (Boxof (Listof Plot)))
  (define frames (box null))
  (define draw! (make-draw! frames))
  (define-values (lhull time _ __) (time-apply algo (list points draw!)))
  (define hull (first lhull))
  (displayln `(got a ,(length hull) hull from ,(length points) points in ,time ms))
  (when (debug)
    (displayln `(with hull ,hull))
    (displayln `(and points ,points)))
  (define v (draw points hull))
  ((inst append (Instance Snip%)) (if (void? v) null (list v)) (unbox frames)))

(: make-draw! : (Boxof (Listof Plot)) -> FrameDrawer)
(define (make-draw! b)
  (λ (pts known . check)
     (define colors (build-list (length check) (λ ([x : Integer]) x)))
     (define v
       (plot
        (list* (points (points->vectors pts))
               (lines (points->vectors known))
               (map (λ ([x : (Sequenceof Point)] [c : Integer])
                       (lines (points->vectors x) #:color c))
                    check
                    colors))))
     (when (not (void? v))
       (set-box! b (cons v (unbox b))))))

(: draw : (Listof Point) (Listof Point) -> Plot)
(define (draw p hull)
  (define v
    (plot 
     (list (points (points->vectors p))
           (lines (points->vectors (append hull (list (first hull))))))))
  (if (not (void? v)) v (error 'internal "should never get here"))) 

(: points->vectors : (Sequenceof Point) -> (Listof (Vectorof Real)))
(define (points->vectors ps)
  (for/list ([p ps])
    (vector (real-part p) (imag-part p))))

(module+ gift-wrap
  (require "algos.rkt")
  (provide test)
  (define (test)
    (define data (random-data))
    (parameterize ([debug #t])
      (draw/timing data gift-wrap))))
