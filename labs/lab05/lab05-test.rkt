#lang racket

(require rackunit "lab05.rkt")

(define add
  (lambda (a b)
    (cond ((number? a) (+ a b))
          ((list? a) (append a b))
          (else (error "unable to add" a b)))))

(define e1  (map list
                 '(     x  y  z + - * cons car cdr nil list add = else )
                 (list 10 20 30 + - * cons car cdr '() list add = #t   )))
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
       (evaluate 'x e1)
       10)

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
       (evaluate '(list 1 2 3) e1)
       (evaluate '(cons 1 (list 2 3)) e1))

(check =
       (evaluate '(if (= 1 1) 2 3) e1)
       2)
(check =
       (evaluate '(if (= 1 2) 2 3) e1)
       3)
(check =
       (evaluate '(cond ((= 1 2) 1)
                        ((= 2 3) 2)
                        (else 3)) e1)
       3)

(check =
       (evaluate '(let ((a 1) (b 2)) (+ a b)) e1)
       3)

(check =
       (evaluate '(+ x (let ((x 100)) (+ x y)) x) e1)
       140)
(check =
       (evaluate '(let ((x 10) (a 20)) (let ((x (+ 2 2)) (y x) (z (* 3 3))) (+ a x y z))) e1)
       43)
(check =
       (evaluate '(let ((x 10)) (+ (let ((x 20)) (+ x x)) x)) e1)
       50)
(check =
       (evaluate '(let ((x 10)) (+ (let ((x (+ x x))) (+ x x)) x)) e1)
       50)
