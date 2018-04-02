#lang racket
(require racket/trace)
(define pow-rec
  (lambda (a b)
    (if (zero? b) 
        1
        (* a (pow-rec a (- b 1))))))

(trace pow-rec)
(pow-rec 2 5)

(define pow-iter
  (lambda (a b)
    (define loop
      (lambda (b product)
        (if (zero? b)
            product
            (loop (- b 1) (* a product)))))
    (trace loop)
    (loop b 1)))

(pow-iter 2 5)

(define pow-iter-2
  (lambda (a b)
    (let loop ((b b) (product 1))
      (if (zero? b)
          product
          (loop (- b 1) (* a product))))))
;(pow-iter-2 2 5)

(define sqr (lambda (x) (* x x)))
(define pow-fast
  (lambda (a b)
    (cond ((zero? b) 1)
          ((even? b) (sqr (pow-fast a (/ b 2))))
          (else (* a (pow-fast a (- b 1)))))))
(trace pow-fast)
(pow-fast 2 5)
(pow-fast 2 100)