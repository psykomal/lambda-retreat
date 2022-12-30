(import "builtins")
(define python builtins)
(import "math")


(define apply-in-underlying-scheme apply)


; eval

(define (eval exp env)
  ;; (display (format "exp** ~a \n" exp))
  (cond ((self-evaluating? exp)
             exp)
        ((variable? exp)
             (lookup-variable-value exp env))
        ((quoted? exp)
             (text-of-quotation exp))
        ((if? exp)
             (eval-if exp env))
        ((definition? exp)
             (eval-definition exp env))
        ((set? exp)
             (eval-set exp env))
        ((begin? exp)
             (eval-sequence (begin-body exp) env))
        ((lambda? exp)
             (eval-lambda exp env))
        ((application? exp)
             (apply (eval (operator exp) env)
                    (list-of-values (operands exp) env) env))
        (else
             (error "Unknown expression type: EVAL" exp))))


(define (list-of-values exps env)
  (map (lambda (e) (eval e env))
       exps))

(define operator car)
(define operands cdr)

;; apply

(define (apply proc arguments env)
  ;; (display (format "*apply* ~a ~a \n" proc arguments))
  (cond ((primitive-procedure? proc)
             (apply-in-underlying-scheme (cadr proc) arguments))
        ((compound-procedure? proc)
             (apply-compound-procedure proc arguments env))
        (else
             (error "unknown procedure" proc))))

(define (apply-compound-procedure proc args env) 
        ;; (display (format "apply-compund* ~a ~a \n" proc args))
        (let (
                (proc-params (procedure-params proc))
                (proc-body (procedure-body proc))
                (proc-env (procedure-env proc)))

                ;; (display (format "proc proc-env* ~a ~a \n" proc proc-env))
                (let ((newenv (extend-env proc-params args proc-env)))
                    (eval proc-body newenv))))



;; eval sequence

(define (eval-sequence exps env)
  (if (null? (cdr exps))
      (eval (car exps) env)
      (begin
       (eval (car exps) env)
       (eval-sequence (cdr exps) env))))


;; type checks

(define (self-evaluating? exp) 
   (number? exp))
(define (variable? exp) (symbol? exp))
(define (quoted? e) (tagged-list? e 'quote ))
(define (definition? exp) (tagged-list? exp 'define))
(define (set? exp) (tagged-list? exp 'set!))
(define (if? exp) (tagged-list? exp 'if))
(define (begin? e) (tagged-list? e 'begin))
(define (application? exp) (pair? exp))

(define (tagged-list? exp tag)
  (and (pair? exp) 
       (eq? (car exp) tag)))


; quote

(define (text-of-quotation e)
  (cadr e))

;; if

(define (eval-if exp env)
  (let ((t (eval (if-test exp) env)))
    (if t (eval (if-consequence exp) env) (eval (if-alternative exp) env))))

(define if-test cadr)
(define if-consequence caddr)
(define if-alternative cadddr)

;; set

(define (set-var exp)
      (cadr exp))
(define (set-body exp)
      (caddr exp))

(define (eval-set exp env) 
  (let ((var (set-var exp))
        (body (set-body exp)))
      (let ((varenv (lookup-env var env)))
            (set-variable! var (eval body env) varenv))))


;; begin


(define (begin-body exp)
  (cdr exp))


;; definition

(define (eval-definition exp env)
  
    (let ((var (definition-var exp)))
        (cond ((variable? var) 
                  (let ((val (eval (definition-value exp) env)))
                       (set-variable! var val env)))

              ((list? var)
                   (let ((params (cdr var))
                         (body (definition-value exp)))
                       (set-variable! (car var) (make-procedure params body env) env)))

              (else (error eval-definition "invalid definition decalaration")))))

(define definition-var cadr)
(define definition-value caddr)



;; procedure

(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (compound-procedure? proc) (tagged-list? proc 'compound-procedure))

(define (make-procedure params body env)
  (list 'compound-procedure params body env)
)

(define (procedure-params proc) 
    (cadr proc))
(define (procedure-body proc)
    (caddr proc))
(define (procedure-env proc)
    (cadddr proc))

;; lambda

(define (lambda? exp) 
      (eq? (car exp) 'lambda))

(define lambda-params cadr)
(define lambda-body caddr)

(define (eval-lambda exp env)
  (make-procedure (lambda-params exp)
                  (lambda-body exp)
                  env))



;; primitives

(define primitives 
  (list
   (list '+ (list 'primitive +))
   (list '* (list 'primitive *))
   (list '- (list 'primitive -))
   (list '/ (list 'primitive /))
   (list '% (list 'primitive %))
   (list '> (list 'primitive >))
   (list '< (list 'primitive <))
   (list '<= (list 'primitive <=))
   (list '= (list 'primitive =))
   (list 'eq? (list 'primitive eq?))))

(define primitive-vars (map car primitives))
(define primitive-vals (map cadr primitives))




;; frame

(define (make-frame variables values)
   (cons variables values))

(define frame-variables car)
(define frame-values cdr)

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (lookup-frame-var var frame)
  (define (scan vars vals)
    (cond ((null? vars) #f)
          ((eq? (car vars) var) (car vals))
          (else (scan (cdr vars) (cdr vals)))))
  (scan (car frame) (cdr frame)))


;; env

(define (make-env)
  '())

(define (empty? env) 
  (or (null? env) (eq? env None)))

(define (extend-env params args env) 
  (let ((new-frame (make-frame params args))) 
    (cons new-frame env)
    ))

(define (frame-env env) (car env))

(define (parent env)
  (if (null? (cdr env))
      None
      (cdr env)))

(define (lookup-env-var var env)
  (cond ((empty? env) #f)
        (else 
             (let ((lookupval (lookup-frame-var var (frame-env env))))
                   (cond ((not lookupval) 
                              (lookup-env-var var (parent env)))
                         (else lookupval))))
  ))


(define (lookup-env var env)
  (cond ((empty? env) #f)
        (else 
             (let ((lookupval (lookup-frame-var var (frame-env env))))
                   (cond ((not lookupval) 
                              (lookup-env var (parent env)))
                         (else env))))
  ))

(define lookup-variable-value lookup-env-var)


(define (set-variable! var val env) 
      ;; (display (format "set-var!** ~a ~a ~a \n" var val env))
      (if (empty? env)
          None
          (add-binding-to-frame! var val (frame-env env))))


(define (setup-env)
  (let ((newenv (make-env)))
    (extend-env primitive-vars primitive-vals newenv)
    ))

(define (top-frame env)
  (cadr env))


;; setup env

(define the-global-env (setup-env))
(define env the-global-env)


(define (try-eval expr)
  (python.print ">" expr)
  (let ((val (eval expr env)))
    (if (not (eq? val '<void>))
        (python.print val))))
