#lang racket

(require rackunit "lab08.rkt" "lab07.rkt")

;; mini test:
(define e2 (list (list '+ +)))
(check equal?
       (parse "(+ 1 2)")
       '(+ 1 2))
(check equal?
       (evaluate (parse "(+ 1 2)") e2)
       3)

;; more tests:
(define add
  (lambda (a b)
    (cond ((and (number? a) (number? b)) (+ a b))
          ((and (list? a) (list? b)) (append a b))
          (else (error "unable to add" a b)))))

(define e1 (map list
                '(x y z + - * cons car cdr nil = equal? < else  add list)
                (list 2 4 6 + - * cons car cdr '() = equal? < #t    add list)))


(check equal?
       (parse "1234")
       1234)

(check equal?
       (parse "fubar")
       'fubar)

(check equal?
       (parse  "(1 2 3)")
       '(1 2 3))

(check equal?
       (parse  "(cons a b)")
       '(cons a b))

(check equal?
       (parse  "()")
       '())

(check equal?
       (parse "(cons (+ 1 2) (list 3 4 5))")
       '(cons (+ 1 2) (list 3 4 5)))

(check equal?
       (parse
        "(let ((f (lambda (a) (lambda (b) (+ a b)))))
                    (let ((g (f 10))
                          (h (f 20)))
                      (list (g 100) (h 100))))")
       '(let ((f (lambda (a) (lambda (b) (+ a b)))))
          (let ((g (f 10))
                (h (f 20)))
            (list (g 100) (h 100))))
       )

(check equal?
       (evaluate (parse "(add 1 2)") e1)
       3)

(check equal?
       (evaluate
        (parse
         "(let ((f (lambda (a) (lambda (b) (+ a b)))))
                    (let ((g (f 10))
                          (h (f 20)))
                      (list (g 100) (h 100))))")
        e1)
       '(110 120))

(check equal?
       (evaluate
        (parse
         "(letrec ((f (lambda (n) (cond ((= n 0) 1) (else (* n (f (- n 1))))))))
             (f 10))")
        e1)
       (* 1 2 3 4 5 6 7 8 9 10))