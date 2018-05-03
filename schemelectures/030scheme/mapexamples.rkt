#lang racket
;; map examples

;; reverse every sublist in a list:
(map reverse '((1 2 3) (a b c) (u v w x y z)))

;; double everything in a list:
(define double-list
  (lambda (ls)
    (apply append
           (map (lambda (x) (list x x))
                ls))))

(double-list '(1 2 3 4 5 a b c))

;; replace a list of numbers with a list of lists of symbols
(define replace-numbers
  (lambda (ls symbol)
    (letrec ((dupe
              (lambda (n symbol)
                (if (zero? n) '()
                    (cons symbol (dupe (- n 1) symbol))))))
      (map (lambda (n) (dupe n symbol))
           ls))))

(replace-numbers '(4 2 3 7) 'foo)
(replace-numbers '(4 2 3 7) '*)