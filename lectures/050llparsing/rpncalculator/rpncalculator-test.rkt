#lang racket

(require rackunit "rpncalculator.rkt")

(check =
       (calc-string "2 2 +")
       4)

(check =
       (calc-string "32 333 +
                  99 *
               22 410 + *")
       (* (+ 410 22) (* 99 (+ 333 32))))

(check =
       (calc-string
        "1 2 3 4 5 6 7 8 9 10 * * * * * * * * *")
       (* 1 2 3 4 5 6 7 8 9 10))
