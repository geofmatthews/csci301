#lang racket

(require rackunit "lab02.rkt")

(check eq?
       #t
       (element-ordered? '(1 2 3) '(1 2 4)))

(check eq?
       #f
       (element-ordered? '(1 2 5) '(1 2 3)))

(check eq?
       #t
       (length-ordered? '(3 4) '(1 2 3)))

(check equal?
       (sort (distribute '0 '((1) (2) (3 4)))
             length-ordered?)
       '((0 1) (0 2) (0 3 4)))

(check equal?
       (sort '((1 1 1 1) (3 2 1) (1 2 3) (5 5))
             length-ordered?)
       '((5 5) (1 2 3) (3 2 1) (1 1 1 1)))

(check equal?
       (subsets '(1 2))
       '(() (1) (2) (1 2)))
(check equal?
       (subsets '(1 2 3 4))
       '(()
         (1)
         (2)
         (3)
         (4)
         (1 2)
         (1 3)
         (1 4)
         (2 3)
         (2 4)
         (3 4)
         (1 2 3)
         (1 2 4)
         (1 3 4)
         (2 3 4)
         (1 2 3 4)))