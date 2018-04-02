#lang racket
; Demonstration of a simple Scheme program
; A procedure to calculate the quadratic formula:
; Given real numbers a, b, c, return a list of two
; real numbers, defined by
; (-b (+-) sqrt(b^2 -4ac))/(2a)
; Smaller is first in the list.

; Although complex numbers are built in to Racket,
; we choose to return NAN when the discriminant
; is less than zero because this procedure is meant
; to be used in a context where imaginary numbers
; are useless.

(provide quadratic)

(define quadratic
  (lambda (a b c)
    (let ((discriminant (- (* b b) (* 4.0 a c))))
      (if (< discriminant 0.0)
          (list +nan.0 +nan.0)
          (two-real-solutions (- b)
                              (sqrt discriminant)
                              (* 2.0 a))))))

(define two-real-solutions
  (lambda (neg-b root-disc two-a)
    (list (/ (- neg-b root-disc) two-a)
          (/ (+ neg-b root-disc) two-a))))