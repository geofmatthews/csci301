#lang racket

(require rackunit "lab07.rkt")

(define add
  (lambda (a b)
    (cond ((and (number? a) (number? b)) (+ a b))
          ((and (list? a) (list? b)) (append a b))
          (else (error "unable to add" a b)))))

(define e1  (map cons
                 '(     x  y  z + - * cons car cdr nil list add = equal? else)
                 (list 10 20 30 + - * cons car cdr '() list add = equal? #t)))
(check =
       (lookup 'x e1)
       10)
(check =
       (lookup 'z e1)
       30)
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
       (evaluate '((lambda (x) (* x x)) 2) e1)
       4)

(check =
       (evaluate '(let ((f (lambda (a) (+ a x)))) (f 100)) e1)
       110)

(check =
       (evaluate '(let ((f (let ((x 100)) (lambda (a) (+ a x))))) (f x)) e1)
       110)

(check equal?
       (evaluate '(let ((f (lambda (a) (lambda (b) (+ a b)))))
                    (let ((g (f 10))
                          (h (f 20)))
                      (list (g 100) (h 100)))) e1)
       '(110 120))

; Factorial without loops or recursion!!!!

(check =
       (evaluate '(let ((f (lambda (f n) (if (= n 0) 1 (* n (f f (- n 1)))))))
                    (f f 10)) e1)
       (* 10 9 8 7 6 5 4 3 2 1))

(check =
       (evaluate '(let ((f (lambda (f n) (if (= n 0) 1 (* n (f f (- n 1)))))))
                    (let ((factorial (lambda (n) (f f n))))
                      (factorial 10))) e1)
       (* 10 9 8 7 6 5 4 3 2 1))
; An alternative approach
(check =
       (evaluate '(let ((f (lambda (f) (lambda (n) (if (= n 0) 1 (* n ((f f) (- n 1)))))))) ((f f) 10)) e1)
       (* 10 9 8 7 6 5 4 3 2 1))
(check =
       (evaluate '(let ((f (lambda (f) (lambda (n) (if (= n 0) 1 (* n ((f f) (- n 1))))))))
                    (let ((factorial (f f)))
                      (factorial 10))) e1)
       (* 10 9 8 7 6 5 4 3 2 1))