#lang racket

; lab 01 solution
; Geoffrey Matthews

(provide make-pi)

(define make-pi
  (lambda (tolerance)
    (pi-loop tolerance 4.0 1.0 0.0)))

; Tail recursive version:
(define pi-loop
  (lambda (tolerance numerator denominator sum)
    (let ((new-term (/ numerator denominator)))
      (if (good-enough? new-term tolerance)
          sum
          (pi-loop
           (new-tolerance tolerance)
           (new-numerator numerator)
           (new-denominator denominator)
           (new-sum sum new-term))))))

(define good-enough?
  (lambda (new-term tolerance)
    (< (abs new-term) tolerance)))
(define new-tolerance
  (lambda (tolerance) tolerance))
(define new-numerator
  (lambda (numerator) (* -1 numerator)))
(define new-denominator
  (lambda (denominator)
    (+ denominator 2.0)))
(define new-sum
  (lambda (sum new-term)
    (+ sum new-term)))

; Non-tail-recursive version:
(define make-pi-2
  (lambda (tolerance)
    (pi-loop-2 tolerance 4.0 1.0)))

(define pi-loop-2
  (lambda (tolerance numerator denominator)
    (let ((new-term (/ numerator denominator)))
      (if (good-enough? new-term tolerance)
          0.0
          (+ new-term
             (pi-loop-2
              (new-tolerance tolerance)
              (new-numerator numerator)
              (new-denominator denominator)
              ))))))