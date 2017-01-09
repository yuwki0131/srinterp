;; a simple meta-circular interpreter
;; curring s-exp

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

(define extend
  (lambda (var val env)
    `((,var . ,val) . ,env)))

(define extend*
  (lambda (vars vals env)
    (append
     (map (lambda (var val) `(,var . ,val)) vars vals)
     env)))

(define-syntax curry
  (syntax-rules ()
    ((_ (a) body)
     (lambda (a) body))
    ((_ (a b ...) body)
     (lambda (a)
       (curry (b ...) body)))))

(define-syntax curry*
  (syntax-rules ()
    ((_ (f args ...))
     (curry (args ...) (f args ...)))))

(define-syntax curry-primitives-2
  (syntax-rules ()
    ((_ f)
     (list (curry* (f x y))))
    ((_ f fs ...)
     (cons (curry* (f x y)) (curry-primitives-2 fs ...)))))

(define initial-env2
  (extend* '(+ - * / mod < > <= >= = eq? equal?)
           (curry-primitives-2
            + - * / mod < > <= >= = eq? equal?) '()))

(define eval2
  (lambda (s-exp env exception)
    (match s-exp
      ((and ('lambda (a) body) lambda-exp)
       `(,lambda-exp . ,env))
      (('if cond-exp then-exp else-exp)
       (if (eval2 cond-exp env exception)
           (eval2 then-exp env exception)
           (eval2 else-exp env exception)))
      ((func-exp arg-exp)
       (let ((arg (eval2 arg-exp env exception))
             (function (eval2 func-exp env exception)))
         (if (procedure? function)
             (function arg)
             (match function
               ((('lambda (var) body) . env)
                (eval2 body (extend var arg env) exception))
               (a
                (exception "# error :: malformed lambda"))))))
      (a
       (cond
        ((or (string? a) (boolean? a) (number? a))
         a)
        ((symbol? a)
         (lookup a env exception))
        (else
         (exception "# error :: malformed object")))))))

(define curry-s-exp
  (lambda (s-exp exception)
    (match s-exp
      (('lambda (a) body)
       `(lambda (,a) ,(curry-s-exp body exception)))
      (('lambda args body)
       `(lambda (,(car args))
          ,(curry-s-exp `(lambda ,(cdr args) ,body) exception)))
      (('if cond-exp then-exp else-exp)
       `(if ,(curry-s-exp cond-exp exception)
            ,(curry-s-exp then-exp exception)
            ,(curry-s-exp else-exp exception)))
      ((fun-exp . args-exp)
       (let ((fun (curry-s-exp fun-exp exception))
             (args (map (lambda (exp) (curry-s-exp exp exception)) args-exp)))
         (fold (lambda (a f) `(,f ,a)) fun args)))
      (a
       (if (or (boolean? a) (symbol? a) (number? a) (string? a))
           a
           (exception "# error :: malformed target program"))))))

(define interp2
  (lambda (s-exp)
    (call/cc
     (lambda (exit-point)
       (eval2 (curry-s-exp s-exp exit-point) initial-env2 exit-point)))))
