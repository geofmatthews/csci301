#lang racket

(require rackunit "quadratic.rkt")

(define same?
  (lambda (a b)
    (< (abs (- a b)) 1.0e-10)))

(define list-same?
  (lambda (ls1 ls2)
    (and (same? (first ls1) (first ls2))
         (same? (second ls1) (second ls2)))))

(check list-same?
       (quadratic 1 2 1) (list -1.0 -1.0))
(check list-same?
       (quadratic 1 0 -1) (list -1.0 1.0))
(check list-same?
       (quadratic 1 -5 6) (list 2.0 3.0))