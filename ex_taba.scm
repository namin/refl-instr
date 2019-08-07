(define cnv
  (lambda (xs ys)
    (define walk
      (lambda (xs)
        (if (null? xs)
            (cons '() ys)
            (let ((rys (walk (cdr xs))))
              (let ((r (car rys))
                    (ys (cdr rys)))
                (cons (cons (cons (car xs) (car ys)) r)
                      (cdr ys)))))))
    (car (walk xs))))
