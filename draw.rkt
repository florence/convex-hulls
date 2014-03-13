#lang typed/racket
(provide draw/timing debug)
(require "shared.rkt"
         (only-in typed/mred/mred Snip%)
         plot/typed
         plot/typed/utils)

(: debug : (Parameterof Any))
(define debug (make-parameter #f))

(define-type Plot (Instance Snip%))

(define call/comp call-with-composable-continuation)

(: draw/timing : (Listof Point) Huller -> (-> (Option Plot)))
(define (draw/timing points algo)
  (: return-tag : (Prompt-Tagof (Option Plot) ((Option Plot) -> (Option Plot))))
  (define return-tag (make-continuation-prompt-tag 'render-return))
  (define-type K (Void -> (Option Plot)))
  (: bnext : (Boxof  (U #f K)))
  (define bnext (box #f))
  (: launch : (-> (Option Plot)) -> (Option Plot))
  (define (launch t)
    (call-with-continuation-prompt t return-tag (ann values (-> (Option Plot) (Option Plot)))))
  (lambda () 
    (: next : (U #f K))
    (define next (unbox bnext))
    (cond
     [next (launch (thunk (next (void))))]
     [else 
      (: draw! : FrameDrawer)
      (define (draw! pts known . check)
        (define p (apply render pts known check))
        (call/comp
         (lambda ([k : K])
           (set-box! bnext k)
           (abort-current-continuation return-tag p))
         return-tag))
      (launch
       (lambda () : False
               (define-values (lhull time _ __) (time-apply algo (list points draw!)))
               (define hull (first lhull))
               (displayln `(got a ,(length hull) hull from ,(length points) points in ,time ms))
               (when (debug)
                 (displayln `(with hull ,hull))
                 (displayln `(and points ,points)))
               (draw! points `(,@hull ,(first hull)))
               #f))])))

(: render : (Sequenceof Point) (Sequenceof Point) (Sequenceof Point) * -> Plot)
(define (render pts known . check)
  (define colors (build-list (length check) (λ ([x : Integer]) x)))
  (define v
    (plot
     (list* (points (points->vectors pts))
            (lines (points->vectors known))
            (map (λ ([x : (Sequenceof Point)] [c : Integer])
                    (lines (points->vectors x) #:color c))
                 check
                 colors))))
  (if (not (void? v))
      v
      (error 'internal "should never get here")))

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
