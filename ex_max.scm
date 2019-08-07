(define (map f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs)))))
(define (max xs) (let ((y 0)) (map (lambda (x) (if (> x y) (set! y x)) y) xs)))
(max '(3 2 4 7 5))
