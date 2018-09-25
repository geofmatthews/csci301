#lang racket

(displayln "counter-------------")
(define counter
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      n)))
(counter)
(counter)
(counter)

(displayln "new-counter ---------")
(define new-counter
  (lambda ()
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        n))))

(define x (new-counter))
(define y (new-counter))
(x)
(x)
(x)
(y)
(x)
(x)
(y)

(displayln "derivatives--------")
(define d
  (lambda (f)
    (let* ((delta 0.00001)
           (two-delta (* 2 delta)))
      (lambda (x)
        (/ (- (f (+ x delta)) (f (- x delta)))
           two-delta)))))

((d (lambda (x) (* x x x))) 5)
((d sin) 0.0)

(displayln "pairs------------")
(define make-pair
  (lambda (first second)
    (lambda (index)
      (cond ((= index 0) first)
            ((= index 1) second)
            (else (error 'bad-index index))))))

(define a1 (make-pair 4 8))
(define a2 (make-pair 100 200))
(define a3 (make-pair a1 a2))

(a1 1)
(a2 0)
((a3 1) 0)

(displayln "summation-------------")
(define sum
  (lambda (a b f)
    (if (> a b) 0
        (+ (f a) (sum (+ a 1) b f)))))
(sum 1 10 (lambda (x) x))
(sum 1 10 (lambda (x) (* x x)))
(sum 1 10 (lambda (x) (* x x x)))


