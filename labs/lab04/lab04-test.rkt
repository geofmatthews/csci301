#lang racket

(require rackunit "lab04.rkt")

(define add
  (lambda (a b)
    (cond ((number? a) (+ a b))
          ((list? a) (append a b))
          (else (error "unable to add" a b)))))

(define e1  (map list
                 '(     x  y  z ls + - * cons car cdr nil list add = nil? else)
                 (list 10 20 30 (list 1 2) + - * cons car cdr '() list add = empty? #t)))

(check =
       (lookup 'x e1)
       10)
(check-exn exn:fail?
           (lambda ()
             (lookup 'foo e1)))
(check =
       (evaluate 'x e1)
       10)

(check-exn exn:fail?
           (lambda ()
             (evaluate '(/ 19 2) e1)))
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
(check equal?
       (special-form? '(if 1 2 3))
       #t)
(check equal?
       (special-form? '(cond a b c))
       #t)
(check equal?
       (special-form? '(cons a b c))
       #f)
(check equal?
       (evaluate-special-form '(if (= 1 2) 3 4) e1)
       4)

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
       (evaluate '(cond ((= 1 2) (+ 1 1))
                        ((= 2 3) (+ 2 2))
                        ((= 3 3) (+ 3 3))
                        (else (+ 4 4))) e1)
       6)
(check =
       (evaluate '(cond ((nil? ls) 1)
                        ((nil? (cdr ls)) 2)
                        ((nil? (cdr (cdr ls))) 3)
                        (else 4)) e1)
       3)