#lang racket
(require racket/gui mzlib/string)
;; language: pretty big
;; Simple program to show cons cells using drscheme
;; Warning!  Will not handle circular lists!
;; DAGs OK, though. (But you can fool the spacing.)
;; Geoffrey Matthews 2004
;;

(define frame (instantiate frame% ("Boxes and Arrows")))

(send frame create-status-line)

(define (msg txt)
  (send frame set-status-text txt))

(define main-panel (instantiate vertical-panel% (frame)))

(define control-panel (instantiate horizontal-panel% (main-panel)
                        (stretchable-height #f)))

(define button-panel (instantiate vertical-panel% (control-panel)
                       (stretchable-width #f)))

(define eval-check
  (instantiate check-box% ()
    (label "Evaluate?")
    (parent button-panel)
    (callback (lambda (c e) '()))))

(define go-button
  (instantiate button% ()
    (label "&Show")
    (parent button-panel)
    (stretchable-width #t)
    (stretchable-height #t)
    (callback 
     (lambda (c e)  
       (let ((result (if (send eval-check get-value)
                         (eval (read-from-string (send text-field get-value)))
                         (read-from-string (send text-field get-value)))))         
         (msg (format "~a" result))         
         (send canvas set-sexpr result)         
         (send canvas refresh))))))

(define text-field   
  (instantiate text-field% ()    
    (label "Expression:")    
    (parent control-panel)    
    (style '(multiple vertical-label))    
    (callback (lambda (tf e)                
                (when (eq? 'text-field-enter (send e get-event-type))                  
                  (send canvas set-sexpr                        
                        (when (send eval-check get-value)                            
                            (eval (read-from-string (send tf get-value)))                            
                            (read-from-string (send tf get-value))))                  
                  (send canvas refresh))))))

(define my-canvas%  
  (class canvas%    
    (override on-paint)    
    (field (sexpr '()) (box-size 25) (items '()))    
    (define/public (set-sexpr x) (set! sexpr x))    
    (define (init-items) (set! items '()))    
    (define (add-item-xy item x y) (set! items (cons (list item x y) items)))    
    (define set-item-xy add-item-xy)    
    (define (item-point item) (assq item items))
        
    (define green-color (make-object color% 0 255 0))    
    (define red-color (make-object color% 255 0 0))    
    (define black-color (make-object color% 0 0 0))    
    (define blue-color (make-object color% 0 0 255))        
    (define white-color (make-object color% 255 255 255))
    
    (define white-brush (make-object brush% white-color 'solid))    
    (define black-pen (make-object pen% black-color 2 'solid))    
    (define red-pen (make-object pen% red-color 2 'solid))    
    (define red-brush (make-object brush% red-color 'solid))    
    (define blue-pen (make-object pen% blue-color 2 'solid))    
    
    (define (dc) (send this get-dc))
    
    (define (draw-box x y)      
      (send (dc) draw-rectangle x y box-size box-size)      
      (send (dc) draw-rectangle (+ x box-size) y box-size box-size))
    
    (define (draw-null x y)      
      (send (dc) draw-line x y (+ x box-size) (+ y box-size))      
      (send (dc) draw-line (+ x box-size) y x (+ y box-size)))
    
    (define (draw-cell c x y)      
      (send (dc) set-pen blue-pen)      
      (send (dc) set-brush white-brush)      
      (draw-box x y)      
      (when (null? (car c)) (draw-null x y))      
      (when (null? (cdr c)) (draw-null (+ x box-size) y)))
        
    (define (sqr x) (* x x))
    
    (define (vlen x1 y1 x2 y2)      
      (sqrt (+ (sqr (- x2 x1)) (sqr (- y2 y1)))))
    
    (define pi (* 2 (asin 1)))
    
    (define (angle x y)      
      (when (zero? x)          
          (/ pi -2))      
      (if (and (> x 0))          
          (atan (/ y x))          
          (+ pi (atan (/ y x)))))
    
    (define (arrowhead x1 y1 x2 y2)      
      (let* ((len (vlen x1 y1 x2 y2))             
             (x (/ (- x2 x1) len))             
             (y (/ (- y2 y1) len))             
             (ang (angle x y))             
             (angoffset (/ pi 8))             
             (hdlen (* box-size -0.5))             
             )        
        (list (make-object point% 0 0)              
              (make-object point%                 
                (* hdlen (cos (+ ang angoffset)))                
                (* hdlen (sin (+ ang angoffset))))              
              (make-object point%                 
                (* hdlen (cos (- ang angoffset)))                
                (* hdlen (sin (- ang angoffset)))))))
    
    
    
    (define (draw-arrow x1 y1 x2 y2)      
      (let ((rad (* box-size 0.1)))        
        (send (dc) set-pen red-pen)        
        (send (dc) set-brush red-brush)        
        (send (dc) draw-arc (- x1 rad) (- y1 rad) (* 2 rad) (* 2 rad) 0 0)
        (send (dc) draw-polygon (arrowhead x1 y1 x2 y2) x2 y2 'odd-even)        
        (send (dc) draw-line x1 y1 x2 y2)))   
    
    (define (draw-list ls x y)      
      (cond ((item-point ls) => cdr)            
            ((pair? ls)             
             (set-item-xy ls x y)             
             (draw-cell ls x y)             
             (when (not (null? (car ls)))               
               (let ((car-xy                      
                      (draw-list (car ls)                                 
                                 x                                 
                                 (+ (* 3 box-size) y))))                 
                 (draw-arrow (+ (* 0.5 box-size) x)                             
                             (+ (* 0.5 box-size) y)                             
                             (car car-xy)                             
                             (cadr car-xy))))             
             (when (not (null? (cdr ls)))               
               (let ((cdr-xy                       
                      (draw-list (cdr ls)                                 
                                 (+ (* 3 box-size (total-width (car ls))) x)                                 
                                 y)))                 
                 (draw-arrow (+ (* 1.5 box-size) x)                             
                             (+ (* 0.5 box-size) y)                             
                             (car cdr-xy)                             
                             (cadr cdr-xy))))             
             (list x y))            
            ((null? ls)             
             (list x y))            
            (else             
             (draw-atom ls x y))))
    
    (define (get-string a)      
      (cond ((number? a) (number->string a))            
            ((symbol? a) (symbol->string a))            
            ((string? a) a)            
            (else "Unknown")))   
    
    (define (draw-atom a x y)      
      (send (dc) set-pen black-pen)      
      (send (dc) set-brush white-brush)
      (send (dc) draw-rectangle x y (* 2 box-size) box-size)
      (send (dc) draw-text (get-string a)
            (+ (* 0.25 box-size) x) 
            (+ (* 0.1 box-size) y))
      (list x y))
    
    (define (total-width item)
      (cond ((null? item) 1)
            ((not (pair? item)) 1)
            ((and (null? (car item)) (null? (cdr item))) 1)
            ((null? (car item)) (+ 1 (total-width (cdr item))))
            ((null? (cdr item)) (max 1 (total-width (car item))))
            (else (+  (total-width (car item)) 
                      (total-width (cdr item))))))
    
    (define (max-depth ls)
      (cond ((null? ls) 1)
            ((pair? ls) (max (add1 (max-depth (car ls))) (max-depth (cdr ls))))
            (else 1)))
    
    (define (on-paint)
      (set! items '())
      (draw-arrow 0 0 box-size box-size)
      (draw-list sexpr box-size box-size))
    
    (super-instantiate ())))

(define canvas (instantiate my-canvas% (main-panel)
                 (min-width 800) (min-height 600)))

(send frame show #t)

