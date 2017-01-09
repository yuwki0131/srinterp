;; a simple meta-circular interpreter
;; shift/reset (delimited continuation)

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

;; (\vky. (k (\v'k'y'. (k' (f v v') y'))))
(define-syntax normal2->cps2
  (syntax-rules ()
    ((_ f)
     (lambda (v k y)
       (k (lambda (vv kk yy) (kk (f v vv) yy)) y)))))

(define-syntax add-transform-cps-2
  (syntax-rules ()
    ((_ () () env)
     env)
    ((_ (var vars ...) (val vals ...) env)
     (extend 'var (normal2->cps2 val)
             (add-transform-cps-2 (vars ...) (vals ...) env)))))

(define initial-env3
  (add-transform-cps-2
   (+ - * / mod < > <= >= = eq? equal?)
   (+ - * / mod < > <= >= = eq? equal?)
   '()))

;; Eval : Exp -> Env -> Cont -> MCont -> Ans
;; Eval[\x. E]rky       = k (\vk'y'.Eval[E]r[x:= v] k' y') y
;; Eval[E1 -> E2|E3]rky	= Eval[E1] r (\by'. b -> Eval[E2]rky' | Eval[E3]rky') y
;; Eval[call/cc c. E]rky= Eval[E] r [c:= (\vk'y'. k v y')] k y
;; Eval[shift c. E]rky  = Eval[E] r [c:= (\vk'y'. k v (\w. k'wy'))] (\xy''.y''x) y
;; Eval[{E}]rky         = Eval[E] r (\xy'. y' x) (\v.kvy)
;; Eval[p E]rky         = Eval[E] r (\vy'. k (p v) y') y
;; Eval[E1 E2]rky       = Eval[E1] r (\fy'. Eval[E2] r (\ay''. f a k y'') y') y
;; Eval[c]rky           = k c y
;; Eval[x]rky           = k (r[x]) y

;; k  : Value -> MCont -> Ans
;; mk : Value -> Ans

(define eval3-gen
  (lambda (exception)
  (define eval3
    (lambda (exp env k y)
      (match exp
        (('lambda (a) body)
         (k (lambda (v kk yy) (eval3 body (extend a v env) kk yy)) y))
        (('if cond-exp then-exp else-exp)
         (eval3 cond-exp env
                (lambda (b yy) (if b (eval3 then-exp env k yy)
                                   (eval3 else-exp env k yy)))
                y))
        (('call/cc (c) body)
         (eval3 body (extend c (lambda (v kk yy) (k v yy)) env) k y))
        (('shift (c) body)
         (eval3 body (extend c (lambda (v kk yy) (k v (lambda (w) (kk w yy)))) env)
                (lambda (x yyy) (yyy x)) y))
        (('reset body)
         (eval3 body env (lambda (x yy) (yy x)) (lambda (v) (k v y))))
        ((e1 e2)
         (eval3 e1 env
                (lambda (f yy)
                  (eval3 e2 env (lambda (a yyy) (f a k yyy)) yy))
                y))
        (a
         (cond
          ((or (boolean? a) (number? a) (string? a))
           (k a y))
         ((symbol? a)
          (k (lookup a env exception) y))
         (else
          (exception "# error :: malformed object")))))))
  eval3))

(define curry-s-exp-with-cont
  (lambda (s-exp exception)
    (match s-exp
      (('lambda (a) body)
       `(lambda (,a) ,(curry-s-exp-with-cont body exception)))
      (('lambda args body)
       `(lambda (,(car args))
          ,(curry-s-exp-with-cont `(lambda ,(cdr args) ,body) exception)))
      (('call/cc (c) body)
       `(call/cc (,c) ,(curry-s-exp-with-cont body exception)))
      (('shift (c) body)
       `(shift (,c) ,(curry-s-exp-with-cont body exception)))
      (('reset body)
       `(reset ,(curry-s-exp-with-cont body exception)))
      (('if cond-exp then-exp else-exp)
       `(if ,(curry-s-exp-with-cont cond-exp exception)
            ,(curry-s-exp-with-cont then-exp exception)
            ,(curry-s-exp-with-cont else-exp exception)))
      ((fun-exp . args-exp)
       (let ((fun (curry-s-exp-with-cont fun-exp exception))
             (args (map (lambda (exp) (curry-s-exp-with-cont exp exception)) args-exp)))
         (fold (lambda (a f) `(,f ,a)) fun args)))
      (a
       (if (or (boolean? a) (symbol? a) (number? a) (string? a) (list? a))
           a
           (exception "# error :: malformed target language"))))))

(define interp3
  (lambda (s-exp)
    (call/cc
     (lambda (exit-point)
       ((eval3-gen exit-point)
        (curry-s-exp-with-cont s-exp exit-point)
        initial-env3 (lambda (v mk) v) (lambda (v) v))))))

(define generate-factorial-sr/p
  (lambda (n)
    `(call/cc (c)
       (+ 1 (reset
             (((lambda (f) ((lambda (p) (f (lambda (a) ((p p) a))))
                            (lambda (p) (f (lambda (a) ((p p) a))))))
               (lambda (y) (lambda (n)
                             (if (= n 1)
                                 1
                                 (if (< n 0)
                                     (c "call/cc exception")
                                     (if (< 50 n)
                                         (shift (c) n)
                                         (* n (y (- n 1))))))))) ,n))))))

(define normal-p
  (generate-factorial-sr/p 10))

(define shift-exception-p
  (generate-factorial-sr/p 100))

(define call/cc-exception-p
  (generate-factorial-sr/p -10))
