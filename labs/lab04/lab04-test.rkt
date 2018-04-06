#lang racket

(require rackunit "lab04.rkt")

(define add
  (lambda (a b)
    (cond ((and (number? a) (number? b)) (+ a b))
          ((and (list? a) (list? b)) (append a b))
          (else (error "unable to add:" a b)))))

(define e1 (map cons
                 '(     x  y  z + - * cons car cdr nil list add)
                 (list 10 20 30 + - * cons car cdr '() list add)))

(check =
       (lookup 'x e1)
       10)
(check =
       (lookup 'z e1)
       30)

(check-exn exn:fail?
           (lambda ()
             (lookup 'foo e1)))
(check =
       (evaluate 'y e1)
       20)

(check-exn exn:fail?
           (lambda ()
             (evaluate '(/ 19 2))))
(check =
       (evaluate '(+ x y) e1)
       30)
(check =
       (evaluate '(+ 3 (- 10 y) z) e1)
       23)
(check =
       (evaluate '(+ x (+ y (+ z 100))) e1)
       160)
(check =
       (evaluate '(* 2 2 2 2 2 (* 2 2 2 2 2)) e1)
       1024)
(check equal?
       (evaluate '(cons 1 2) e1)
       '(1 . 2))
(check equal?
       (evaluate '(cons 1 (cons 2 (cons 3 nil))) e1)
       '(1 2 3))
(check =
       (evaluate '(add 1 2) e1)
       3)
(check equal?
       (evaluate '(add (cons 1 nil) (cons 2 nil)) e1)
       (list 1 2))
(check equal?
       (evaluate '(list 1 2 z) e1)
       (evaluate '(cons 1 (list 2 z)) e1))