;;(load-option 'format)

(define print-stack
  (lambda (es)
    (let ((m "~26A~26A~%"))
      (for-each
       (lambda (e)
         (let ((n (car e))
               (down (car (cdr e)))
               (up (car (cdr (cdr e)))))
           
           (format #t m (cons n down) up)
           (format #t m "|" "^")
           (format #t m "V" "|")))
       es)

      (format #t "~A~%" (make-string 27 #\-)))))
