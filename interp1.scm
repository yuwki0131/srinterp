;; a simple meta-circular interpreter

(use util.match)

(define lookup
  (lambda (var env exception)
    (cond
     ((null? env)
      (exception (string-append
                  "# error :: could not find the variable > "
                  (symbol->string var))))
     ((eq? var (caar env))
      (cdar env))
     (else
      (lookup var (cdr env) exception)))))

(define extend*
  (lambda (vars vals env)
    (append
     (map (lambda (var val) `(,var . ,val)) vars vals)
     env)))

(define initial-env
  (extend* '(+ - * / mod < > <= >= = eq? equal? null? number? car cdr cons)
           (list + - * / mod < > <= >= = eq? equal? null? number? car cdr cons)
           '()))

(define eval1
  (lambda (s-exp env exception)
    (match s-exp
      ((and ('lambda args body) lambda-exp)
       `(,lambda-exp . ,env))
      (('if cond-exp then-exp else-exp)
       (if (eval1 cond-exp env exception)
           (eval1 then-exp env exception)
           (eval1 else-exp env exception)))
      ((func-exp . args-exps)
       (let ((args (map (lambda (exp) (eval1 exp env exception)) args-exps))
             (function (eval1 func-exp env exception)))
         (if (procedure? function)
             (apply function args)
             (match function
               ((('lambda vars body) . env)
                (eval1 body (extend* vars args env) exception))
               (a
                (display a)
                (exception "# error :: malformed lambda"))))))
      (a
       (cond
        ((or (string? a) (boolean? a) (number? a))
         a)
        ((symbol? a)
         (lookup a env exception))
        (else
         (exception "# error :: malformed object")))))))

(define interp1
  (lambda (s-exp)
    (call/cc
     (lambda (exit-point)
       (eval1 s-exp initial-env exit-point)))))

;; sample programs

(define factorial-p
	'(((lambda (f) ((lambda (p) (f (lambda (a) ((p p) a))))
                  (lambda (p) (f (lambda (a) ((p p) a))))))
     (lambda (y) (lambda (n) (if (< n 1) 1 (* n (y (- n 1))))))) 100))

(define fib-p
	'(((lambda (f) ((lambda (p) (f (lambda (a) ((p p) a))))
                  (lambda (p) (f (lambda (a) ((p p) a))))))
     (lambda (fib)
       (lambda (n) (if (< n 1) 1 (+ (fib (- n 1)) (fib (- n 2))))))) 10))
