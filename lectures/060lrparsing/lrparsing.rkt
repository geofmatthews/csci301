#lang racket
;; LR parsing
;; Geoffrey Matthews 2005
;;
;; IMPORTANT:  turn on case sensitivity to use a and A

(define testing #f)

(define mlist (lambda args
  (if (null? args) '()
      (mcons (car args) (apply mlist (cdr args))))))

(define mappend (lambda (arg1 arg2)
                  (if (null? arg1) (mapply mlist arg2)
                      (mcons (mcar arg1) (mappend (mcdr arg1) arg2)))))

(define un-m (lambda (mls)
               (if (null? mls) mls
                   (cons (mcar mls) (un-m (mcdr mls))))))

(define in-m (lambda (ls)
               (if (null? ls) ls
                   (mcons (car ls) (in-m (cdr ls))))))

(define mapply (lambda (func args)
                     (if (list? args) 
                         (apply func args)
                         (apply func (un-m args)))))

(define mreverse (lambda (arg)
                   (if (null? arg) arg
                       (mappend (mcdr arg) (mlist (mcar arg))))))

;; A configuration holds the input and the stack:
(define (make-config input) (mlist (mlist 0) (mappend (in-m input) (in-m '($)))))
(define (stack config) (mcar config))
(define (input config) (mcar (mcdr config)))
(define (state config) (mcar (stack config)))
(define (char config) (mcar (input config)))

(define (shift! config) 
  (set-mcar! config  (mcons (mcar (input config)) (stack config)))
  (set-mcar! (mcdr config) (mcdr (input config))))
(define (push! config a)
  (set-mcar! config (mcons a (stack config))))
(define (pop! config)
  (set-mcar! config (mcdr (stack config))))

(define (print-config config)
  (print (list (mreverse (stack config)) (input config)))
  (newline))

(define (action table state symbol)
  (let ((rule (assoc symbol (vector-ref table state))))
    (if rule (car (cdr rule)) 'reject)))

(define (shift-reduce table action config)
  (print-config config)
  (cond ((number? action) (push! config action))
        ((eq? action 'accept) 'accept)
        ((eq? action 'reject) 'reject)
        ((eq? (car action) 'shift) (shift! config) (push! config (cadr action)))
        (else (reduce table config action))))

(define (reduce table config rule)
  (display "Reduce by: ") (map print rule) (newline)
  (let loop ((rhs (reverse (cddr rule))))
    (cond ((null? rhs)  
           (let ((newstate (action table (state config) (car rule))))
             (if (eq? newstate 'reject) 'reject
                 (begin
                   (push! config (car rule))
                   (push! config newstate)))))
          ((eq? (mcar (mcdr (stack config))) (car rhs))
           (pop! config)
           (pop! config)
           (loop (cdr rhs)))
          (else 'reject))))

(define (step table config)
  (let ((act (action table (state config) (char config))))
    (shift-reduce table act config)))

(define (parse table input)
  (let ((config (make-config input)))
    (do ((result (step table config) (step table config)))
      ((member result '(accept reject)) result))))


;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;

(define fig12-2
; S -> aCd | bCD     C -> cC|c      D -> d
  #(((a (shift 1)) (b (shift 6)) (S 10))
    ((c (shift 4)) (C 2))
    ((d (shift 3)))
    (($ (S -> a C d)))
    ((c (shift 4)) (d (C -> c)) (C 5))
    ((d (C -> c C)))
    ((c (shift 4)) (C 7))
    ((d (shift 8)) (D 9))
    (($ (D -> d)))
    (($ (S -> b C D)))
    (($ accept))))
(if testing (begin
              (print 'fig12-2) (newline)            
              (parse fig12-2 '(b c c d))
              (parse fig12-2 '(a c c c d))
              (parse fig12-2 '(a c c c b))))

(define fig12-6
;  S -> lambda| aSb
  #(((a (shift 1)) ($ (S -> )) (S 2))
    ((a (shift 1)) (b (S -> )) (S 3))
    (($ accept))
    ((b (shift 4)))
    ((b (S -> a S b)) ($ (S -> a S b)))))
(if testing (begin
              (print 'fig12-6) (newline)
              (parse fig12-6 '(a a b b))
              (parse fig12-6 '(a a a b b))
              (parse fig12-6 '(a a b b b))))
         
(define test
;  S -> AS|B     A -> a      B -> b
  #(((a (shift 1)) (b (shift 2)) (B 3) (A 4) (S 6) )
    ((a (A -> a)) (b (A -> a)))
    (($ (B -> b)))
    (($ (S -> B)))
    ((a (shift 1)) (b (shift 2)) (A 4) (B 3) (S 5))
    (($ (S -> A S)))
    (($ accept))))
(if testing (begin
              (print 'test) (newline)
              (parse test '(a a a b))
              (parse test '(a a a ))))

(define test2
;  S -> SA|B     A -> a     B -> b
  #(((b (shift 2)) (B 3) (S 4))
    (($ (A -> a)) (a (A -> a)))
    ((a (B -> b)) ($ (B -> b)))
    ((a (S -> B)) ($ (S -> B)))
    ((a (shift 1)) (A 5) ($ accept))
    ((a (S -> S A)) ($ (S -> S A)))
    ))
(if testing (begin
         (print 'test2) (newline)
         (parse test2 '(b a a a))
         (parse test2 '(b a a a b))))

(define ll
;  S -> ASB|BSA|C     A -> a    B -> b    C -> c
  #(((a (shift 1)) (A 2) (b (shift 3)) (B 4) (c (shift 9)) (C 10) (S 11))
    ((a (A -> a)) (b (A -> a)) (c (A -> a)) ($ (A -> a)))
    ((a (shift 1)) (b (shift 3)) (c (shift 9)) (A 2) (B 4) (C 10) (S 5))
    ((a (B -> b)) (b (B -> b)) (c (B -> b)) ($ (B -> b)))
    ((a (shift 1)) (b (shift 3)) (c (shift 9)) (A 2) (B 4) (C 10) (S 6))
    ((a (shift 1)) (b (shift 3)) (B 7))
    ((a (shift 1)) (b (shift 3)) (A 8))
    ((a (S -> A S B)) (b (S -> A S B)) ($ (S -> A S B)))
    ((a (S -> B S A)) (b (S -> B S A)) ($ (S -> B S A)))
    ((a (C -> c)) (b (C -> c)) ($ (C -> c)))
    ((a (S -> C)) (b (S -> C)) ($ (S -> C)))
    (($ accept))))

(if testing (begin
         (print 'll) (newline)
         (parse ll '(a a b c a b b))
         (parse ll '(c))
         (parse ll '(b b a c b a a))
         (parse ll '(a b c b a))))