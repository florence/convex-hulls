#lang typed/racket
(provide gift-wrap)
(require typed/rackunit)

(define-type Point Complex)
(define-type Huller ((Listof Point) -> (Listof Point)))

;; gift-wrapping
(: gift-wrap : Huller)
(define (gift-wrap points)
  (define spoints (list->set points))
  (define s (get-leftmost spoints))
  (let loop ([results (list s)] [remainder spoints])
    (define endpoint (first results))
    (define next
      (for/fold: : Point ([best : Point (set-first remainder)])
                 ([candidate : Point (set-rest remainder)])
        (if (more-to-left? candidate best endpoint)
            candidate
            best)))
    (if (= next s)
        results
        (loop (cons next results) (set-remove remainder next)))))

(module+ test
  (check-equal? (gift-wrap '(0 10+10i 9-12i 1+.5i))
                (reverse '(0 10+10i 9-12i))))

(: get-leftmost : ((Setof Point) -> Point))
(define (get-leftmost points)
  (for/fold: ([l : Point (set-first points)]) ([p : Point (set-rest points)])
    (if (< (real-part l) (real-part p)) l p)))

(: more-to-left? : (Point Point Point -> Boolean))
(define (more-to-left? c b e)
  (: get-angle : (Point -> Real))
  (define (get-angle p)
    (define a (angle p))
    (if (= a pi) (- pi) a))
  (and (not (= c e))
       (or 
        (= b e)
        (let* ([b0 (- b e)]
               [c0 (- c e)]
               [β  (get-angle b0)]
               [κ  (get-angle c0)])
          (displayln `(b0: ,b0 c0: ,c0))
          (displayln `(κ: ,κ β: ,β))
          (displayln "")
          (> κ β)))))



(: normalize : (Point -> Point))
(define (normalize p)
  (/ p (magnitude p)))

(module+ test
  (check-true  (more-to-left? 5+5i 0+10i 10+10i) "backwards horrizontal")
  (check-true  (more-to-left? 9-12i 0 10+10i) "inverse")
  (check-false (more-to-left? 0 9-12i 10+10i) "normal")
  (check-false (more-to-left? 47 3+100i 1+88i) "complex")
  (check-false (more-to-left? -5-10i 0-10i 0) "downward line")
  (check-false (more-to-left? -4-10i 0+10i 0) "directly up")
  (check-false (more-to-left? 0+88i 33+18i 96+68i) "up back from example")
  (check-false (more-to-left? -1+1i -1-1i 0) "up back")
  (check-true  (more-to-left? -1-1i -1+1i 0) "up back reversed")
  (check-false (more-to-left? 1-10i 1+10i 0) "a")
  (check-false (more-to-left? 1+0i 0+1i 0) "b") 
  (check-true  (more-to-left? 0 10 10-10i) "c"))



