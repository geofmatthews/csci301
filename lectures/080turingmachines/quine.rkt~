
(define data (quote (a b "foo")))
(begin  
  (define (print-data-as-data data)
    (newline)
    (display "(define data (quote ")
    (write data)
    (display "))")
    (newline))
  
  (define (print-data-as-code data)
    (write data))
  
  (print-data-as-data data)
  
  (print-data-as-code data)
  
  (newline)
)