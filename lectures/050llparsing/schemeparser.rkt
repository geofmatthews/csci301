#lang racket
; Simple demonstration of recursive descent parsing
; of Scheme syntax
;
; Geoffrey Matthews
; 2018

(provide string->input parse parse-number parse-symbol parse-list)

(define string->input
  (lambda (str)
    (let ((ls (string->list str)))
      (lambda (command)
        (cond ((eq? command 'first)
               (car ls))
              ((eq? command 'empty?)
               (empty? ls))
              ((eq? command 'next)
               (set! ls (cdr ls)))
              (else
               (error "unknown parser command:" command)))))))

(define parse
  (lambda (si)
    (cond ((si 'empty?)
           (error "can't parse empty"))
          ((char-whitespace? (si 'first))
           (si 'next)
           (parse si))
          ((eq? (si 'first) #\()
           (si 'next)
           (parse-list si))
          ((char-numeric? (si 'first))
           (parse-number si))
          (else
           (parse-symbol si)))))

(define char->number
  (lambda (c)
    (- (char->integer c) (char->integer #\0))))

(define char-punctuation?
  (lambda (c)
    (or (char-whitespace? c)
        (eq? c #\()
        (eq? c #\)))))

(define parse-number
  (lambda (si)
    (let loop ((n 0))
      (cond
        ((si 'empty?) n)
        ((not (char-numeric? (si 'first))) n)
        (else
         (let ((new-n (+ (* n 10) (char->number (si 'first)))))
           (si 'next)
           (loop new-n)))))))

(define parse-symbol
  (lambda (si)
    (let loop ((ls '()))
      (cond
        ((si 'empty?)
         (string->symbol (list->string (reverse ls))))
        ((char-punctuation? (si 'first))
         (string->symbol (list->string (reverse ls))))
        (else
         (let ((new-ls (cons (si 'first) ls)))
           (si 'next)
           (loop new-ls)))))))

(define parse-list
  (lambda (si)
    (cond ((si 'empty?)
           (error "cannot parse-list empty string"))
          ((eq? (si 'first) #\))
           (si 'next)
           '())
          (else
           (cons (parse si)
                 (parse-list si))))))