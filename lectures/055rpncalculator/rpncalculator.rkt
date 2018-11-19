#lang racket
; Simple demonstration of RPN calculator parsing
; and evaluating.  We can use recursive descent because
; the language of RPN is LL(1)
;
; Geoffrey Matthews
; 2018

(provide calc-string calc)

;; We use a global variable for the stack.
;; This would be better done as an object,
;; but we want to avoid extra scheme syntax.
(define *the-stack* '())
(define push
  (lambda (n) (set! *the-stack* (cons n *the-stack*))))
(define pop
  (lambda () 
    (when (null? *the-stack*)
      (error "Empty stack!"))
    (let ((item (car *the-stack*)))
      (set! *the-stack* (cdr *the-stack*))
      item)))
(define add
  (lambda ()
    (push (+ (pop) (pop)))))
(define mul
  (lambda ()
    (push (* (pop) (pop)))))
(define sub
  (lambda ()
    (push (+ (- (pop)) (pop)))))
(define div
  (lambda ()
    (let* ((a (pop)) (b (pop)))
      (push (/ b a)))))
;; End of abstract object *the-stack*

;; Another global for the input
(define *the-input* '())
(define set-input!
  (lambda (s)
    (when (not (string? s))
      (error "Invalid input:" s))
    (set! *the-input*
          (string->list s))))
(define empty-input? (lambda () (null? *the-input*)))
(define current-char (lambda ()
                       (when (empty? *the-input*)
                         (error "Empty input!"))
                       (car *the-input*)))
(define skip-char (lambda () (set! *the-input* (cdr *the-input*))))
;; End of the input object

(define calc
  (lambda ()
    (let loop ()
      (set-input! (read-line))
      (display "Result: ")
      (display (calc-list))
      (newline)
      (loop))))

(define calc-string
  (lambda (s)
    (set-input! s)
    (calc-list)))

(define calc-list
  (lambda ()
    (cond ((empty-input?)
           (pop))
          ((char-whitespace? (current-char))
           (skip-char)
           (calc-list))
          ((eq? (current-char) #\+)
           (skip-char)
           (add)
           (calc-list))
          ((eq? (current-char) #\*)
           (skip-char)
           (mul)
           (calc-list))
          ((eq? (current-char) #\-)
           (skip-char)
           (sub)
           (calc-list))
          ((eq? (current-char) #\/)
           (skip-char)
           (div)
           (calc-list))
          ((char-numeric? (current-char))
           (parse-number 0))
          (else
           (error "Cannot handle input." (list->string *the-input*))))))

(define char->number
  (lambda (c)
    (- (char->integer c) (char->integer #\0))))

(define parse-number
  (lambda (n)
    (cond
      ((empty-input?)
       (push n)
       (calc-list))
      ((not (char-numeric? (current-char)))
       (push n)
       (calc-list))
      (else
       (let ((new-n (char->number (current-char))))
         (skip-char)
         (parse-number (+ (* n 10) new-n)))))))

