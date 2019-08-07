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
    ((eq? (car exp) 'set!) (eval-set! exp env cont))
    ((eq? (car exp) 'lambda) (eval-lambda exp env cont))
    ((eq? (car exp) 'begin) (eval-begin (cdr exp) env cont))
    ((eq? (car exp) 'let) (eval-let (cadr exp) (cddr exp) env cont))
    (else (eval-application exp env cont))))

(define (eval-var exp env cont)
  (let ((pair (get exp env)))
    (if (pair? pair)
        (cont (cdr pair) `(var ,exp ,(cdr pair)))
        (error 'eval-var (list 'unbound 'variable: exp)))))

(define (eval-quote exp env cont)
  (cont (cadr exp) `(quote ,exp)))

(define (eval-if exp env cont)
  (let ((pred-part (cadr exp))
        (then-part (caddr exp))
        (else-part (cdddr exp)))
    (base-eval pred-part env (lambda (p pi)
      (cond (p (base-eval then-part env (lambda (r ri) (cont r `(if-then ,p ,pi ,r ,ri)))))
            ((null? else-part) (cont #f `(if-else-empty ,p ,pi)))
            (else (base-eval (car else-part) env (lambda (r ri) (cont r `(if-else ,p ,pi ,r ,ri))))))))))

(define (eval-define exp env cont)
  (if (pair? (cadr exp))
      (let ((var (caadr exp))
            (body (cons 'lambda (cons (cdadr exp)
                                 (cddr exp)))))
        (base-eval body env (lambda (data di)
          (define-value var data env)
          (cont var `(define ,var ,data ,di)))))
      (let ((var (cadr exp))
            (body (caddr exp)))
        (base-eval body env (lambda (data di)
          (define-value var data env)
          (cont var `(define ,var ,data ,di)))))))

(define (eval-set! exp env cont)
  (let ((var (cadr exp))
        (body (caddr exp)))
    (base-eval body env (lambda (data di)
      (let ((pair (get var env)))
        (if (pair? pair)
            (begin (set-value! var data env)
                   (cont var `(set! ,var ,data ,di)))
            (error 'eval-set! (list 'unbound 'variable var))))))))

(define lambda-tag (cons 'lambda 'tag))
(define (eval-lambda exp env cont)
  (let ((lambda-body (cddr exp))
        (lambda-params (cadr exp)))
    (cont (list lambda-tag lambda-params lambda-body env) `(lambda ,lambda-params ,lambda-body ,env))))

(define (eval-begin body env cont)
  (define (eval-begin-local trace body)
    (if (null? (cdr body))
        (base-eval (car body) env
                   (lambda (x xi) (cont x `(begin ,x ,(append trace (list xi))))))
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
        (cont r `(let ,params ,operand ,oi ,r ,ri))))))))

(define (eval-list exp env cont)
  (if (null? exp)
      (cont '() '(nil))
      (base-eval (car exp) env (lambda (val1 vi1)
        (eval-list (cdr exp) env (lambda (val2 vi2)
          (cont (cons val1 val2) `(cons ,(list val1 vi1) ,(list val2 vi2)))))))))

(define (eval-application exp env cont)
  (eval-list exp env (lambda (l li)
    (base-apply (car l) (cdr l) env (lambda (r ri)
    (cont r `(app ,r ,ri ,l ,li)))))))

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
                 (lambda (r ri) (cont r `(lambda-body ,lambda-params ,operand ,r ,ri))))
               (error 'base-apply (list 'wrong 'number 'of 'arguments: operand 'to: lambda-params)))))
        (else (error 'base-apply (list 'not 'a 'function: operator)))))

(define (show-val v)
  (cond ((and (pair? v) (eq? (car v) lambda-tag))
         `(lambda ,(cadr v) ,(caddr v)))
        ((pair? v)
         (cons (show-val (car v)) (show-val (cdr v))))
        (else v)))

(define (caddddr x) (cadddr (cdr x)))
(define (cadddddr x) (caddddr (cdr x)))
(define (cadaddr x) (car (cdr (caddr x))))

(define (show-instr s d pre post i)
  (let ((p (assq (car i) pre)))
    (when p (set-car! s ((cdr p) (car s) d i)))
    (let ((r
           (cond ((eq? (car i) 'number)
                  `(number ,(cadr i)))
                 ((eq? (car i) 'var)
                  `(var ,(cadr i) ,(show-val (caddr i)))
                  )
                 ((eq? (car i) 'quote)
                  `(quote ,(cadr i)))
                 ((eq? (car i) 'if-then)
                  `(if-then ,(show-val (cadr i)) ,(show-instr s d pre post (caddr i))
                            ,(show-val (cadddr i)) ,(show-instr s d pre post (caddddr i))))
                 ((eq? (car i) 'if-else-empty)
                  `(if-else-empty ,(show-val (cadr i)) ,(show-instr s d pre post (caddr i))))
                 ((eq? (car i) 'if-else)
                  `(if-else ,(show-val (cadr i)) ,(show-instr s d pre post (caddr i))
                            ,(show-val (cadddr i)) ,(show-instr s d pre post (caddddr i))))
                 ((eq? (car i) 'define)
                  `(define ,(cadr i) ,(show-val (caddr i)) ,(show-instr s d pre post (cadddr i))))
                 ((eq? (car i) 'set!)
                  `(set! ,(cadr i) ,(show-val (caddr i)) ,(show-instr s d pre post (cadddr i))))
                 ((eq? (car i) 'lambda)
                  `(lambda ,(cadr i) ,(caddr i)))
                 ((eq? (car i) 'begin)
                  `(begin ,(show-val (cadr i)) ,(map (lambda (xi) (show-instr s d pre post xi)) (caddr i))))
                 ((eq? (car i) 'let)
                  `(let ,(cadr i) ,(show-val (caddr i)) ,(show-instr s d pre post (cadddr i)) ,(show-val (caddddr i)) ,(show-instr s (1+ d) pre post (cadddddr i))))
                 ((eq? (car i) 'nil)
                  `(nil))
                 ((eq? (car i) 'cons)
                  `(cons (,(show-val (caadr i)) ,(show-instr s d pre post (cadadr i)))
                         (,(show-val (caaddr i)) ,(show-instr s d pre post (cadaddr i)))))
                 ((eq? (car i) 'app)
                  `(app ,(show-val (cadr i)) ,(show-instr s d pre post (caddr i))
                        ,(show-val (cadddr i)) ,(show-instr s d pre post (caddddr i))))
                 ((eq? (car i) 'apply-proc)
                  `(apply-proc ,(show-val (cadr i)) ,(show-val (caddr i)) ,(show-val (cadddr i))))
                 ((eq? (car i) 'lambda-body)
                  `(lambda-body ,(cadr i) ,(show-val (caddr i)) ,(show-val (cadddr i)) ,(show-instr s (1+ d) pre post (caddddr i))))
                 (else (error 'show-instr s d pre post (list 'unknown i))))))
      (let ((p (assq (car i) post)))
        (when p (set-car! s ((cdr p) (car s) d i)))
        (or (car s) r)))))

(define (indent n)
  (string->symbol (make-string (1+ n) #\*)))

(define show-instr-pre (list
  (cons 'var (lambda (s d i) (let ((v (caddr i)))
                          (unless (or (and (pair? v) (eq? (car v) lambda-tag))
                                      (procedure? v))
                            (pretty-print `(,(indent d) var ,(cadr i) ,(show-val (caddr i))))) #f)))
  (cons 'set! (lambda (s d i) (pretty-print `(,(indent d) set! ,(cadr i) ,(show-val (caddr i)))) #f))
  (cons 'lambda-body (lambda (s d i) (pretty-print `(,(indent d) lambda... ,(cadr i) ,(show-val (caddr i)) ,(show-val (cadddr i)))) #f))))

(define show-instr-post (list))

(define (display-instr ri)
  (show-instr (list #f) 0 show-instr-pre show-instr-post ri))

(define print-stack
  (lambda (es)
    (let ((m "~25a~25a\n"))
      (map
       (lambda (e)
         (let ((n (car e))
               (down (car (cdr e)))
               (up (car (cdr (cdr e)))))
           
           (printf m (cons n down) up)
           (printf m "|" "^")
           (printf m "V" "|")))
       es)

      (printf "~26,,,'_a\n" ""))))

(define show-instr-pre-taba (list
  (cons 'lambda-body (lambda (s d i) (pretty-print `(,(indent d) push ,(show-val (caddr i)))) (cons (list '- (show-val (caddr i)) (show-val (cadddr i))) s)))))
(define show-instr-post-taba (list
  (cons 'lambda-body (lambda (s d i) (pretty-print `(,(indent d) pop ,(show-val (cadddr i)))) s))))
(define (display-instr-taba ri)
  (print-stack (show-instr (list '()) 0 show-instr-pre-taba show-instr-post-taba ri)))

;(set! display-instr display-instr-taba)

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
  (display "instr> ")
  (let ((r (read)))
    (if (eq? r 'exit) 'exit
        (base-eval
         r
         env
         (lambda (r ri)
           (display r)
           (newline)
           (display-instr ri)
           (newline)
           (repl-inner env))))))

(define (repl)
  (repl-inner (copy init-env)))
