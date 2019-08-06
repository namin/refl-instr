;; reifies the runtime structure of a computation
;; passed around as the second argument to a continuation, after the result
;;
;; base from https://github.com/readevalprintlove/black/blob/master/black.scm
;; but stripped of tower architecture

(define (base-eval exp env cont)
  (cond
    ((number? exp) (cont exp `(number ,exp)))
    ((boolean? exp) (cont exp `(boolean ,exp)))
    ((string? exp) (cont exp `(string ,exp)))
    ((symbol? exp) (eval-var exp env cont))
    ((eq? (car exp) 'quote) (eval-quote exp env cont))
    ((eq? (car exp) 'if) (eval-if exp env cont))
    ((eq? (car exp) 'define) (eval-define exp env cont))
    ((eq? (car exp) 'lambda) (eval-lambda exp env cont))
    ((eq? (car exp) 'begin) (eval-begin (cdr exp) env cont))
    (else (eval-application exp env cont))))

(define (eval-var exp env cont)
  (let ((pair (get exp env)))
    (if (pair? pair)
        (cont (cdr pair) `(var ,exp))
        (error 'eval-var (list 'unbound 'variable: exp)))))

(define (eval-quote exp env cont)
  (cont (cadr exp) `(quote ,exp)))

(define (eval-if exp env cont)
  (let ((pred-part (cadr exp))
        (then-part (caddr exp))
        (else-part (cdddr exp)))
    (base-eval pred-part env (lambda (p pi)
      (cond (p (base-eval then-part env (lambda (r ri) (cont r `(if-then ,pi ,ri)))))
            ((null? else-part) (cont #f `(if-else-empty ,pi)))
            (else (base-eval (car else-part) env (lambda (r ri) (cont r `(if-else ,pi ,ri))))))))))

(define (eval-define exp env cont)
  (if (pair? (cadr exp))
      (let ((var (caadr exp))
            (body (cons 'lambda (cons (cdadr exp)
                                 (cddr exp)))))
        (base-eval body env (lambda (data di)
          (define-value var data env)
          (cont var `(define ,var)))))
      (let ((var (cadr exp))
            (body (caddr exp)))
        (base-eval body env (lambda (data di)
          (define-value var data env)
          (cont var `(define ,var)))))))

(define lambda-tag (cons 'lambda 'tag))
(define (eval-lambda exp env cont)
  (let ((lambda-body (cddr exp))
        (lambda-params (cadr exp)))
    (cont (list lambda-tag lambda-params lambda-body env) `(lambda ,exp))))

(define (eval-begin body env cont)
  (define (eval-begin-local body)
    (if (null? (cdr body))
        (base-eval (car body) env cont)
        (base-eval (car body) env
                   (lambda (x) (eval-begin-local (cdr body))))))
  (if (null? body)
      (error 'eval-begin '(eval-begin: null body))
      (eval-begin-local body)))

(define (eval-list exp env cont)
  (if (null? exp)
      (cont '() '())
      (base-eval (car exp) env (lambda (val1 vi1)
        (eval-list (cdr exp) env (lambda (val2 vi2)
          (cont (cons val1 val2) `(cons ,vi1 ,vi2))))))))

(define (eval-application exp env cont)
  (eval-list exp env (lambda (l li)
    (base-apply (car l) (cdr l) env (lambda (r ri)
    (cont r `(app ,ri ,li)))))))

(define (base-apply operator operand env cont)
  (cond ((procedure? operator)
         (let ((r (apply operator operand)))
           (cont r `(apply-proc ,r ,operator ,operand))))
        ((and (pair? operator) (eq? (car operator) lambda-tag))
         (let ((lambda-params (cadr operator))
               (lambda-body (caddr operator))
               (lambda-env (cadddr operator)))
           (if (can-receive? lambda-params operand)
               (eval-begin lambda-body (extend lambda-env lambda-params operand)
                 (lambda (r ri) (cont r `(lambda-body ,ri))))
               (error 'base-apply (list 'wrong 'number 'of 'arguments: operand 'to: lambda-params)))))
        (else (error 'base-apply (list 'not 'a 'function: operator)))))

(define init-env (list (list
  (cons '+ +)
  (cons '- -)
  (cons '* *)
  (cons '= =)
  (cons '< <)
  (cons '> >))))

(define (repl-inner env)
  (display "> ")
  (let ((r (read)))
    (if (eq? r 'exit) 'exit
        (base-eval
         r
         env
         (lambda (r ri)
           (pp r)
           (pp ri)
           (repl-inner env))))))

(define (repl)
  (repl-inner (copy init-env)))
