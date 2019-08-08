;; an additive architecture to plug-in instrumentations in an interpreter
;;
;; base from https://github.com/readevalprintlove/black/blob/master/black.scm
;; but stripped of tower architecture

(define plugs
  (list))

(define (plug-in tag ds is)
  (let ((p (assq tag plugs)))
    (if p ((cdr p) ds is) is)))

(define (base-eval exp env cont)
  (cond
    ((number? exp) (cont exp (plug-in 'number (list exp) '())))
    ((boolean? exp) (cont exp `(boolean ,exp)))
    ((string? exp) (cont exp `(string ,exp)))
    ((symbol? exp) (eval-var exp env cont))
    ((eq? (car exp) 'quote) (eval-quote exp env cont))
    ((eq? (car exp) 'if) (eval-if exp env cont))
    ((eq? (car exp) 'define) (eval-define exp env cont))
    ((eq? (car exp) 'set!) (eval-set! exp env cont))
    ((eq? (car exp) 'lambda) (eval-lambda exp env cont))
    ((eq? (car exp) 'begin) (eval-begin (cdr exp) env cont))
    ((eq? (car exp) 'let) (eval-let (cadr exp) (cddr exp) env cont))
    (else (eval-application exp env cont))))

(define (eval-var exp env cont)
  (let ((pair (get exp env)))
    (if (pair? pair)
        (cont (cdr pair) (plug-in 'var (list exp (cdr pair)) '()))
        (error 'eval-var (list 'unbound 'variable: exp)))))

(define (eval-quote exp env cont)
  (cont (cadr exp) (plug-in 'quote (list exp) '())))

(define (eval-if exp env cont)
  (let ((pred-part (cadr exp))
        (then-part (caddr exp))
        (else-part (cdddr exp)))
    (base-eval pred-part env (lambda (p pi)
      (cond (p (base-eval then-part env (lambda (r ri) (cont r (plug-in 'if-then (list p r) (list pi ri))))))
            ((null? else-part) (cont #f (plug-in 'if-else-empty (list p) (list pi))))
            (else (base-eval (car else-part) env (lambda (r ri) (cont r (plug-in 'if-else (list p r) (list pi ri)))))))))))

(define (eval-define exp env cont)
  (if (pair? (cadr exp))
      (let ((var (caadr exp))
            (body (cons 'lambda (cons (cdadr exp)
                                 (cddr exp)))))
        (base-eval body env (lambda (data di)
          (define-value var data env)
          (cont var (plug-in 'define (list var data) (list di))))))
      (let ((var (cadr exp))
            (body (caddr exp)))
        (base-eval body env (lambda (data di)
          (define-value var data env)
          (cont var (plug-in 'define (list var data) (list di))))))))

(define (eval-set! exp env cont)
  (let ((var (cadr exp))
        (body (caddr exp)))
    (base-eval body env (lambda (data di)
      (let ((pair (get var env)))
        (if (pair? pair)
            (begin (set-value! var data env)
                   (cont var (plug-in 'set! (list var data) (list di))))
            (error 'eval-set! (list 'unbound 'variable var))))))))

(define lambda-tag (cons 'lambda 'tag))
(define (eval-lambda exp env cont)
  (let ((lambda-body (cddr exp))
        (lambda-params (cadr exp)))
    (cont (list lambda-tag lambda-params lambda-body env) (plug-in 'lambda (list lambda-params lambda-body env) '()))))

(define (eval-begin body env cont)
  (define (eval-begin-local trace body)
    (if (null? (cdr body))
        (base-eval (car body) env
                   (lambda (x xi) (cont x (plug-in 'begin (list x) (append trace (list xi))))))
        (base-eval (car body) env
                   (lambda (x xi) (eval-begin-local (append trace (list xi)) (cdr body))))))
  (if (null? body)
      (error 'eval-begin '(eval-begin: null body))
      (eval-begin-local '()  body)))

(define (eval-let pairs body env cont)
  (let ((params (map car pairs))
        (args (map cadr pairs)))
    (eval-list args env (lambda (operand oi)
      (eval-begin body (extend env params operand) (lambda (r ri)
                             (cont r (plug-in 'let (list params operand r) (list oi ri)))))))))

(define (eval-list exp env cont)
  (if (null? exp)
      (cont '() (plug-in 'nil '() '()))
      (base-eval (car exp) env (lambda (val1 vi1)
        (eval-list (cdr exp) env (lambda (val2 vi2)
          (cont (cons val1 val2) (plug-in 'cons (list val1 val2) (list vi1 vi2)))))))))

(define (eval-application exp env cont)
  (eval-list exp env (lambda (l li)
    (base-apply (car l) (cdr l) env (lambda (r ri)
      (cont r (plug-in 'app (list exp  r l) (list ri li))))))))

(define (base-apply operator operand env cont)
  (cond ((procedure? operator)
         (let ((r (apply operator operand)))
           (cont r (plug-in 'apply-proc (list r operator operand) '()))))
        ((and (pair? operator) (eq? (car operator) lambda-tag))
         (let ((lambda-params (cadr operator))
               (lambda-body (caddr operator))
               (lambda-env (cadddr operator)))
           (if (can-receive? lambda-params operand)
               (eval-begin lambda-body (extend lambda-env lambda-params operand)
                 (lambda (r ri) (cont r (plug-in 'lambda-body (list lambda-params operand r) (list ri)))))
               (error 'base-apply (list 'wrong 'number 'of 'arguments: operand 'to: lambda-params)))))
        (else (error 'base-apply (list 'not 'a 'function: operator)))))

(define (show-val v)
  (cond ((and (pair? v) (eq? (car v) lambda-tag))
         `(lambda ,(cadr v) ,(caddr v)))
        ((pair? v)
         (cons (show-val (car v)) (show-val (cdr v))))
        (else v)))

(define (indent n)
  (string->symbol (make-string (1+ n) #\*)))

(define instr-plugs
  (list
   (cons 'var (lambda (ds is)
                (let ((v (cadr ds)))
                  (if (or (and (pair? v) (eq? (car v) lambda-tag))
                          (procedure? v))
                      '()
                      (list `(var ,is ,(car ds) ,(show-val (cadr ds))))))))
   (cons 'set! (lambda (ds is)
                 (list `(set! ,is ,(car ds) ,(show-val (cadr ds))))))
   (cons 'lambda-body (lambda (ds is)
                   (list `(call ,is with ,(car ds) ,(map show-val (cadr ds)) ret ,(show-val (caddr ds))))))))

(set! plugs instr-plugs)

(define (display-instr d i)
  (map (lambda (x) (cond ((null? i))
                    ((and (pair? x) (symbol? (car x)))
                     (display `(,(indent d) ,(car x) . ,(cddr x)))
                     (newline)
                     (for-each (lambda (y) (display-instr (1+ d) y)) (cadr x)))
                    (else (display-instr (1+ d) x))))
       i))

(define taba-plugs
  (list
   (cons 'app (lambda (ds is)
                (if (and (pair? (car is)) (eq? (caaar is) 'call))
                    (list (list 'app is (caar ds) (caddddr (caar is)) (caddddr (cddr (caar is)))))
                    is)))
   (cons 'lambda-body (lambda (ds is)
                   (list `(call ,is with ,(car ds) ,(map show-val (cadr ds)) ret ,(show-val (caddr ds))))))))

(define (traverse-taba s i)
  (for-each (lambda (x) (cond ((null? i))
                         ((and (pair? x) (symbol? (car x)))
                          (when (eq? (car x) 'app)
                            (set-car! s (cons (cddr x) (car s))))
                          (for-each (lambda (y) (traverse-taba s y)) (cadr x)))
                         (else (traverse-taba s x))))
            i)
  (car s))

(load "stack.scm")
(define (display-taba d i)
  (print-stack (reverse (traverse-taba (list (list)) i))))

;(set! plugs taba-plugs)
;(set! display-instr display-taba)

(define init-env (list (list
  (cons '+ +)
  (cons '- -)
  (cons '* *)
  (cons '= =)
  (cons '< <)
  (cons '> >)
  (cons 'cons cons)
  (cons 'car car)
  (cons 'cdr cdr)
  (cons 'null? null?)
)))

(define (repl-inner env)
  (display "instr2> ")
  (let ((r (read)))
    (if (eq? r 'exit) 'exit
        (base-eval
         r
         env
         (lambda (r ri)
           (display r)
           (newline)
           (display-instr 0 ri)
           (newline)
           (repl-inner env))))))

(define (repl)
  (repl-inner (copy init-env)))
