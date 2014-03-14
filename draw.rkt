#lang typed/racket
(provide draw/timing debug)
(require "shared.rkt"
         (only-in typed/mred/mred Snip%)
         plot/typed
         plot/typed/utils)

(: debug : (Parameterof Any))
(define debug (make-parameter #f))

(define-type Plot (Instance Snip%))
(define-type OPlot (Option Plot))
(define-type K (Void -> OPlot))

(: return-tag : (Prompt-Tagof OPlot (OPlot -> OPlot)))
(define return-tag (make-continuation-prompt-tag 'render))

(: draw/timing : (Listof Point) Huller -> (-> OPlot))
(define (draw/timing pts algo)
  (define bnext : (Boxof (Option K))
    (box #f))
  (define draw! (make-draw! bnext))
  (: launch : (-> OPlot) -> OPlot)
  (define (launch t) 
    (call-with-continuation-prompt t return-tag (ann values (OPlot -> OPlot))))
  (lambda () 
    (define next : (Option K)
      (unbox bnext))
    (launch 
     (cond
      [next (thunk (next (void)))]
      [else
       (lambda () 
         (define-values (lhull time _ __) (time-apply algo (list pts draw!)))
         (define hull (first lhull))
         (when (debug)
           (displayln `(got a ,(length hull) hull from ,(length pts) points in ,time ms))
           (displayln `(with hull ,hull))
           (displayln `(and points ,pts)))
         (draw! pts `(,@hull ,(first hull)))
         #f)]))))

(: make-draw! : ((Boxof (Option K)) -> FrameDrawer))
(define ((make-draw! bnext) pts known . check) 
  (define p (apply render pts known check))
  (call-with-composable-continuation
   (lambda ([k : K])
     (set-box! bnext k)
     (abort-current-continuation return-tag p))
   return-tag))

(: render : (Sequenceof Point) (Sequenceof Point) (Sequenceof Point) * -> Plot)
(define (render pts known . check)
  (define colors (build-list (length check) (λ ([x : Integer]) (* 2 x))))
  (define xmax (apply max (map real-part (sequence->list pts))))
  (define ymax (apply max (map imag-part (sequence->list pts))))
  (define xmin (apply min (map real-part (sequence->list pts))))
  (define ymin (apply min (map imag-part (sequence->list pts))))  
  (define xshift (* .2 (- xmax xmin)))
  (define yshift (* .2 (- ymax ymin)))
  (define v
    (plot
     (list* (points (points->vectors pts))
            (lines (points->vectors known))
            (map (λ ([x : (Sequenceof Point)] [c : Integer])
                    (lines (points->vectors x) #:color c))
                 check
                 colors))
     #:x-max (+ xmax xshift)
     #:y-max (+ ymax yshift)
     #:x-min (- xmin xshift) 
     #:y-min (- ymin yshift)))
  (if (not (void? v))
      v
      (error 'internal "should never get here")))

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
