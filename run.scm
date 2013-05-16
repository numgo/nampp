#lang eopl

(require "parse.scm" "class.scm" "misc.scm" "store.scm" "format.scm")
;; ======================================================================

(define runfile
  (lambda (file)
    (let ((string (readfile file)))
      (display string)
      (display (format "~a" (expval->string (run (readfile file)))))
      (newline))))

(define run
  (lambda (string)
    (value-of-program (scan&parse string))))

(define value-of-program
  (lambda (pgm)
    (initialize-store!)
    (cases program pgm
      (a-program (class-decls body)
        (initialize-class-env! class-decls)
        (trace-class);클래스 정보 출력
        (value-of body (init-env))))))

(define value-of
  (lambda (exp env)
    (trace-run-enter exp env)
    (let ((val (value-of-1 exp env)))
      (trace-run-exit val)
      val)))

(define value-of-1
  (lambda (exp env)
    (cases expression exp
      (const-exp (num)
        (num-val num))
      (var-exp (var)
        (deref (apply-env env var)))
      (zero?-exp (exp1)
        (let ((num1 (expval->num (value-of exp1 env))))
          (if (zero? num1)
            (bool-val #t)
            (bool-val #f))))
      (diff-exp (exp1 exp2)
        (let ((num1 (expval->num (value-of exp1 env)))
              (num2 (expval->num (value-of exp2 env))))
          (num-val (- num1 num2))))
      (if-exp (exp1 exp2 exp3)
        (let ((val1 (value-of exp1 env)))
          (if (expval->bool val1)
            (value-of exp2 env)
            (value-of exp3 env))))
	;; 용규 수정;;
      (let-exp (vars exps body)
        (let ((vlist (value-of-exps exps env)))
          (value-of body (extend-env* vars (value-of-refs vlist) env))))
      (proc-exp (vars body)
        (proc-val (procedure vars body env)))
      (call-exp (rator rand)
        (let ((proc (expval->proc (value-of rator env)))
              (arg (value-of rand env)))
          (apply-procedure proc arg)))
      (letrec-exp (p-names b-varss p-bodies letrec-body)
        (value-of letrec-body
          (extend-env-rec* p-names b-varss p-bodies env)))
      (begin-exp (exp1 exps)
        (letrec ((value-of-begins
              (lambda (exp2 es)
                (let ((v1 (value-of exp2 env)))
                  (if (null? es)
                    v1
                    (value-of-begins (car es) (cdr es)))))))
          (value-of-begins exp1 exps)))
      (assign-exp (var exp1)
        (begin
          (setref!
            (apply-env env var)
            (value-of exp1 env))
          (deref (apply-env env var))))
      (sum-exp (exp1 exp2)
        (let ((num1 (expval->num (value-of exp1 env)))
              (num2 (expval->num (value-of exp2 env))))
          (num-val (+ num1 num2))))

;; 2013. 3. 28 용규 수정
;; list-exp의 interpreter
;; list-exp은 lst의 각각의 value들을 list로 만들어준다..
;; ex) ((3 -3) (5 -5))
      (list-exp (lsts)
        (let ((list-of-value
              (map (lambda (lst) (value-of lst env)) lsts)))
          (list-val list-of-value)))
;;; Class 관련

;; self-exp는 자기 자신의 obj-val를 return 한다
      (self-exp ()
        (obj-val (apply-env env '%self)))

;; super-call-exp는 super identifier ({Expression}*) 을 호출할 때 처리하는 interpreter
;; expressions들의 expvals을 args에, 그리고 self object를 obj에 저장한뒤
;; apply-method procedure를 호출한다. (super class에서 method-name에 해당하는 method와 self, 그리고 args를 인자로 받는다)
      (super-call-exp (method-name rands)
        (let ((args (value-of-exps rands env))
              (obj (apply-env env '%self)))
          (apply-method
            (find-method (apply-env env '%super) method-name)
            obj args)))

;; new-object-exp는 new Identifier ({Expression}*)을 호출할 때 처리하는 interpreter
;; expressions들의 expvals를 args에, new-object를 통해 새로 만든 객체를 obj에 저장한 뒤
;; apply-method procedure를 호출한다 (class에서 initialize에 해당하는 method와 해당 클래스 객체, 그리고 args를 인자로 받는다)
;; 그리고 해당 obj-val를 return 한다
      (new-object-exp (class-name rands)
	  (let ((args (value-of-exps rands env))
                (obj (new-object class-name)))
            (apply-method (find-method class-name 'initialize)
              obj args)
            (obj-val obj)))

;; method-call-exp는 new Identifier ({Expression}*)을 호출할 때 처리하는 interpreter
;; expression들의 expvals를 args에, obj-exp의 value인 obj-val을 obj에 저장한 뒤
;; apply-method procedure를 호출한다 (해당 object를 만든 class에서 method-name에 해당하는 method와 obj, 그리고 args를 인자로 받는다)
      (method-call-exp (obj-exp method-name rands)
        (let ((args (value-of-exps rands env))
              (obj (value-of obj-exp env)))
          (apply-method
            (find-method (object->class-name (expval->obj obj)) method-name)
            (expval->obj obj) args)))
      (else
        (eopl:error 'value-of-1 "arg=~a" exp)))))

;; procedure에서 새로운 env(extend-env)를 만들어서 body expression을 실행시키는 apply-procedure이다.
(define apply-procedure
  (lambda (proc1 val)
    (cases proc proc1
      (procedure (var body saved-env)
        (value-of body (extend-env var (newref val) saved-env)))
      (else
        (eopl:error 'apply-procedure "arg=~a" proc1)))))

;; 용규 make ;;
;; let-exp (vars exps body) 사용 함수
;; return (ref1 ref2 ref3 ...)

(define value-of-refs
 (lambda (exps)
  (map (lambda (exp) (newref exp)) exps)))

;; lex-exp (vars exps body) 사용함수
;; return (var1 var2 var3 ...)

(define value-of-exps
 (lambda (exps env)
  (map (lambda (exp) (value-of exp env)) exps)))


;; ======================================================================

;; method에 있는 body를 실행하기 위해 environment를 만들어준다
;; method를 호출한 객체의 field들부터 초기화 해준 후 super class의 환경을 추가한다
;; 그 다음 method에 있는 expression을 새로운 envrionment를 통해 실행한다. 
(define apply-method
  (lambda (m self args)
    (cases method m
      (a-method (vars body super-name field-names)
        (let ((new-env (empty-env)))
          (begin
            (if (not (null? field-names)) ;; method에 field-names들이 null이라면
              (set! new-env (extend-env* field-names (object->fields self) new-env))
              'aaaaaa)
            (set! new-env (extend-env-with-self-and-super self super-name new-env))
            (if (not (null? vars))
              (set! new-env (extend-env* vars (map newref args) new-env))
              'aaaaaaaaaaaaaaaaaaaaaa)
            (value-of body new-env)))))))

