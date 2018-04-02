#lang racket

(require rackunit "schemeparser.rkt")

(check equal?
       (parse (string->input "1234()"))
       1234)

(check equal?
       (parse (string->input "fubar 99"))
       'fubar)

(check equal?
       (parse (string->input "(1 2 3)"))
       '(1 2 3))

(check equal?
       (parse (string->input "(cons a b)"))
       '(cons a b))

(check equal?
       (parse (string->input "()"))
       '())

(check equal?
       (parse (string->input "(cons (+ 1 2) (list 3 4 5))"))
       '(cons (+ 1 2) (list 3 4 5)))

(check equal?
       (let ((si (string->input "(+ 3 (+ 4 5)) 948() (a b c)")))
         (list (parse si)
               (parse si)
               (parse si)
               (parse si)))
       '((+ 3 (+ 4 5))
         948
         ()
         (a b c)))

(check equal?
       (parse
        (string->input
         "(let ((f (lambda (a) (lambda (b) (+ a b)))))
                    (let ((g (f 10))
                          (h (f 20)))
                      (list (g 100) (h 100))))"))
       '(let ((f (lambda (a) (lambda (b) (+ a b)))))
                    (let ((g (f 10))
                          (h (f 20)))
                      (list (g 100) (h 100))))
       )