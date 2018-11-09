#lang racket

(require rackunit "lab07.rkt")

(define add
  (lambda (a b)
    (cond ((and (number? a) (number? b)) (+ a b))
          ((and (list? a) (list? b)) (append a b))
          (else (error "unable to add" a b)))))

(define e1 (map list
                 '(x y z + - * cons car cdr nil empty? = equal? < else  add list)
             (list 2 4 6 + - * cons car cdr '() empty? = equal? < #t    add list)))
(check =
       (lookup 'x e1)
       2)
(check =
       (lookup 'z e1)
       6)
(check =
       (lookup 'x e1)
       2)
(check-exn exn:fail?
           (lambda ()
             (lookup 'foo e1)))
(check =
       (evaluate 'x e1)
       2)

(check-exn exn:fail?
           (lambda ()
             (evaluate '(/ 19 2))))
(check =
       (evaluate '(+ x y) e1)
       6)
(check =
       (evaluate '(+ 3 (- 10 y) z) e1)
       15)
(check =
       (evaluate '(+ x (+ y (+ z 100))) e1)
       112)
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
       108)
(check =
       (evaluate '((lambda (x) (* x x)) 2) e1)
       4)

(check =
       (evaluate '(let ((f (lambda (a) (+ a x)))) (f 100)) e1)
       102)

(check =
       (evaluate '(let ((f (let ((x 100)) (lambda (a) (+ a x))))) (f x)) e1)
       102)

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

; And now with letrec:
(check =
       (evaluate '(letrec ((f (lambda (n) (if (< n 1) 1 (* n (f (- n 1))))))) (f 5)) e1)
       120)
(check eq?
       (evaluate '(letrec ((even? (lambda (n) (if (= n 0) (= 1 1) (odd? (- n 1)))))
                           (odd? (lambda (n) (if (= n 0) (= 1 2) (even? (- n 1)))))
                           (plus (lambda (a b) (if (= a 0) b (+ 1 (plus (- a 1) b))))))
                    (even? (plus 4 5))) e1)
       #f)

(check =
       (evaluate '(let ((f (lambda (x) (* 3 x)))) (let ((f (lambda (x) (f (f x))))) (f 2))) e1)
       18)
(check =
       (evaluate '(letrec ((f (lambda (x) (if (= 0 x) 1 (* x (f (- x 1)))))))
                    (f (let ((f (lambda (x) (* 3 x))))
                         (let ((f (lambda (x) (f (f x))))) (f 2)))))
                 e1)
       (* 18 17 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1))
(check =
       (evaluate '(letrec ((f (lambda (x) (if (= 0 x) 1 (+ x (f (- x 1)))))))
                    (f
                     (let ((f (lambda (x) (f (+ x 2)))))
                       (f
                        (let ((f (lambda (x) (f (+ x 3)))))
                          (f 3)))))) e1)
       305372)

(check equal?
       (evaluate
        '(letrec ((append
                   (lambda (a b)
                     (if (empty? a) b
                         (cons (car a)
                               (append (cdr a) b))))))
           (append (list 1 2 3) (list 4 5 6)))
        e1)
       '(1 2 3 4 5 6))

(check equal?
       (evaluate
        '(letrec ((reverse
                   (lambda (ls)
                     (if (empty? ls) nil
                         (append (reverse (cdr ls))
                                 (cons (car ls) nil)))))
                  (append
                   (lambda (a b)
                     (if (empty? a) b
                         (cons (car a)
                               (append (cdr a) b))))))
           (reverse (list 1 2 3 4 5)))
        e1)
       '(5 4 3 2 1))

(check equal?
       (evaluate
        '(letrec
             ((map
               (lambda (f ls)
                 (if (empty? ls)
                     nil
                     (cons (f (car ls))
                           (map f (cdr ls)))))))
           (map
            (lambda (x) (list x (* 2 x)))
            (list 1 2 3 4)))
        e1)
       '((1 2) (2 4) (3 6) (4 8)))

