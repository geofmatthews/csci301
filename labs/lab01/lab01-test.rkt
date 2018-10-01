#lang racket

(require rackunit "lab01.rkt")

;; Note that the same? predicate uses the
;; same tolerance as the tolerance for make-pi

(define same?
  (lambda (a b tolerance)
    (< (abs (- a b)) tolerance)))

(check (lambda (a b) (same? a b 0.01))
       (make-pi 0.01)
       pi)
(check (lambda (a b) (same? a b 0.001))
       (make-pi 0.001)
       pi)
(check (lambda (a b) (same? a b 0.0001))
       (make-pi 0.0001)
       pi)

;; Using a for loop makes these a bit simpler:
(for-each 
 (lambda (tolerance)
   (check (lambda (a b) (same? a b tolerance))
          (make-pi tolerance)
          pi))
 (list 0.01 0.001 0.0001)
 )