#lang typed/racket

(provide gift-wrap)

(define-type Point Complex)
(define-type Huller ((Listof Point) -> (Listof Point)))

;; gift-wrapping
(: gift-wrap : Huller)
(define (gift-wrap points)
  (define spoints (list->set points))
  (define s (get-leftmost spoints))
  (let loop ([results (list s)] [reference (- s 5)])
    (define endpoint (first results))
    (define next
      (for/fold: : Point ([best : Point (set-first spoints)])
                 ([candidate : Point (set-rest spoints)])
        (if (or (= best endpoint)
                (= best reference)
                (more-to-left candidate best endpoint reference))
            candidate
            best)))
    (if (= next s)
        results
        (loop (cons next results) endpoint))))

(: get-leftmost : ((Setof Point) -> Point))
(define (get-leftmost points)
  (for/fold: ([l : Point (set-first points)]) ([p : Point (set-rest points)])
    (if (< (real-part l) (real-part p)) l p)))

(: more-to-left : (Point Point Point Point -> Boolean))
(define (more-to-left c b e r)
  (and (not (= c e))
       (not (= c r))
       (> (angle-from c e r) (angle-from b e r))))


(: angle-from : (Point Point Point -> Real))
(define (angle-from n m b)
  (define base (- m b))
  (define point (- n b))
  (- (atan (imag-part point) (real-part point))
     (atan (imag-part base) (real-part base))))

(: dot : (Complex Complex -> Real))
(define (dot a b)
  (+ (* (real-part a) (real-part b))
     (* (imag-part a) (imag-part b))))
