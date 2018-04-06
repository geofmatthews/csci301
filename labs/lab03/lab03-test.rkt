#lang racket

(require rackunit "lab03.rkt")

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