#lang racket
(define *input* '(a a a b c c c $))
(define *error* #f)

(define (S1)
  (cond 
    ((front? 'a)
     (displayln "S -> aSc")
     (match 'a)
     (S1)
     (match 'c))
    ((front? 'b)
     (displayln "S -> b")
     (match 'b))
    (else
     (error))))

(define (error) (set! *error* #t))
(define (front? letter) (and (not (null? *input*))
                             (equal? (car *input*) letter)))
(define (match letter)
  (cond ((front? letter)
         (set! *input* (cdr *input*)))
        (else
         (error))))
(begin
  (S1)
  (when (not (front? '$)) (error))
  (if *error*
      (displayln "Error!")
      (displayln "Success!")))

(set! *input* '(a a a c c c c b b b $))
(set! *error* #f)

(define (S2)
  (cond ((front? 'a)
         (A)
         (S2)
         (match 'b))
        ((front? 'c) 
         (C))
        ((front? '$)
         (C))
        (else (error))))
(define (A)
  (cond ((front? 'a)
         (match 'a))
        (else (error))))
(define (C)
  (cond ((front? 'b)
         )
        ((front? 'c)
         (match 'c)
         (C))
        ((front? '$)
         )
        (else (error))))

(begin
  (S2)
  (when (not (front? '$)) (error))
  (if *error*
      (displayln "Error!")
      (displayln "Success!")))