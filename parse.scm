#lang eopl

(require "format.scm")
(provide program program? a-program
         class-decl class-decl? a-class-decl
         method-decl method-decl? a-method-decl
         expression expression? const-exp var-exp zero?-exp diff-exp if-exp let-exp proc-exp call-exp letrec-exp begin-exp assign-exp sum-exp list-exp new-object-exp method-call-exp self-exp super-call-exp
         scan&parse pgm->string class-decl->string method-decl->string list-var->string exp->string rands-expression let->exp)
;; ======================================================================

;; typed-object-oriented 언어 추가 된 expression
;; 2013. 5. 14(화) 수정자 김용규, 박승주
;; 수정내용
;; 1. ppt에 맞게 문법 순서 정리1
;; 2. 새로 추가 된 문법 추가


;; a-program(class-decls body)의 context free grammar
;; Program ::= {ClassDecl}* Expression
(define-datatype program program?
  (a-program (class-decls (list-of class-decl?)) (body expression?)))

;; a-class-decl(class-name super-name interface-names field-types field-names method-decls)의 cfg
;; ClassDecl ::= class Identifier extends Identifier {implements Identifier}*
;;               {field Type Identifier}* {MethodDecl}*
(define-datatype class-decl class-decl?
  (a-class-decl (class-name symbol?) (super-name symbol?) (interface-names (list-of symbol?))
                (field-types (list-of Type?)) (field-names (list-of symbol?))
                (method-decls (list-of method-decl?)))

;; an-interface-decl(interface-name abs-method-decls)의 cfg
;; ClassDecl ::= interface Identifier {AbstractMethodDecl}*
  (an-interface-decl (interface-name symbol?) (abs-method-decls (list-of abstract-method-decl?))))
   
;; a-method-decl(result-type method-name vars var-types body)의 cfg
;; MethodDecl ::= method Type Identifier({Identifier : Type}*(,)) Expression

(define-datatype method-decl method-decl?
  (a-method-decl (result-type Type?) (method-name symbol?) 
                 (vars (list-of symbol?)) (var-types (list-of Type?)) (body expression?)))

;; an-abstract-method-decl(result-type m-name m-vars m-var-types)의 cfg
;; AbstractMethodDecl ::= method Type Identifier({Identifier : Type}*(,))

(define-datatype abstract-method-decl abstract-method-decl?
   (an-abstract-method-decl (result-type Type?) (m-name symbol?)
                            (m-vars (list-of symbol?)) (m-var-types (list-of Type?))))

(define-datatype expression expression?
;; const-exp(num)의 context free grammar
;; Expression ::= Number
  (const-exp (num number?))
  
;; var-exp(var)의 context free grammar
;; Expression ::= Identifier
  (var-exp (var symbol?))
  
;; zero-exp(exp1)의 cfg
;; Expression ::= zero?(Expression)  
  (zero?-exp (exp1 expression?))
  
;; if-exp(exp1 exp2 exp3)의 cfg
;; Expression ::= if Expression then Expression else Expression
  (if-exp (exp1 expression?) (exp2 expression?) (exp3 expression?))
  
;; assign-exp(var exp1)의 cfg
;; Expression ::= set Identifier = Expression  
  (assign-exp (var symbol?) (exp1 expression?))
  
;; begin-exp(exp1 exps)의 cfg
;; Expression ::= begin Expression {; Expression}* end
  (begin-exp (exp1 expression?) (exps (list-of expression?)))
  
;; sum-exp(exp1 exp2), diff-exp(exp1 exp2), mul-exp(exp1 exp2)
;; div-exp(exp1 exp2), mod-exp(exp1 exp2)의 context free grammar 
;; Expression ::= +(Expression, Expression)                  
;; Expression ::= -(Expression, Expression)                  
;; Expression ::= *(Expression, Expression)                                    
;; Expression ::= /(Expression, Expression)                                    
;; Expression ::= %(Expression, Expression)
  (sum-exp (exp1 expression?) (exp2 expression?))
  (diff-exp (exp1 expression?) (exp2 expression?))
  (mul-exp (exp1 expression?) (exp2 expression?))
  (div-exp (exp1 expression?) (exp2 expression?))
  (mod-exp (exp1 expression?) (exp2 expression?))
  
;; list-exp(lst)의 context free grammar
;; Expression ::= list({Expression}*(,))                  
  (list-exp (lst (list-of expression?)))
  
;; lex-exp(vars exps body)의 cfg
;; Expression ::= let {Identifier = Expression}* in Expression  
  (let-exp (vars (list-of symbol?)) (exps (list-of expression?)) (body expression?))
  
;; call-exp(rator rands)의 cfg
;; Expression ::= Expression({Expression}*(,))
  (call-exp (rator expression?) (rands (list-of expression?)))

;; proc-exp(vars types body)의 cfg
;; Expression ::= proc({Identifier : Type}*(,)) Expression
  (proc-exp (vars (list-of symbol?)) (types (list-of Type?)) (body expression?))

;; letrec-exp(p-result-types p-names b-varss b-var-typess p-bodies letrec-body)의 cfg
;; Expression ::= letrec{Type Identifier ({Identifier : Type}*(,)) = Expression}* in Expression  
  (letrec-exp (p-result-types Type) (p-names symbol?) (b-varss (list-of (list-of symbol?)))
              (b-var-typess (list-of (list-of Type?))) (p-bodies (list-of expression?)) 
              (letrec-body expression?))

;; new-object-exp (class-name rands)의 cfg
;; Expression ::= new Identifier({Expression}*(,))
  (new-object-exp (class-name symbol?) (rands (list-of expression?)))

;; method-call-exp(obj-exp method-name rands)의 cfg
;; Expression ::= send Expression Identifier({Expression}*(,))  
  (method-call-exp (obj-exp expression?) (method-name symbol?) (rands (list-of expression?)))
  
;; self-exp()의 cfg
;; Expression ::= self  
  (self-exp)
  
;; super-call-exp(method-name rands)의 cfg  
  (super-call-exp (method-name symbol?) (rands (list-of expression?)))
                  
                  
;; cast-exp(obj-exp c-name)의 cfg
;; Expression ::= cast Expression Identifier
  (cast-exp (obj-exp expression?) (c-name symbol?))              
  
;; instanceof-exp(exp name)의 cfg
;; Expression ::= instanceof Expression Identifier  
  (instanceof-exp (exp expression?) (name symbol?)))
;; ======================================================================

(define-datatype Type Type?
;; int-type()의 cfg
;; Type ::= int  
  (int-type)
  
;; bool-type()의 cfg
;; Type ::= bool  
  (bool-type)
  
;; proc-type(arg-types result-type)의 cfg  
;; Type ::= ({Type}+(,) -> Type)  
  (proc-type (arg-types (list-of Type?)) (result-type Type?))
  
;; void-type()의 cfg
;; Type ::= void  
  (void-type)
  
;; class-type(class-name)의 cfg  
;; Type ::= Identifier  
  (class-type (class-name symbol?))
  
;; list-type(type1)의 cfg  
;; Type ::= listof Type
  (list-type (type1 Type?)))
  
(define scanner-rules
  '((whitespace (whitespace) skip)
    (comment ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit "_" "?" "-"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)))

(define parser-rules
  ;; Program ::= {ClassDecl}* Expression
  '((program ((arbno class-decl) expression) a-program)

    ;; ClassDecl ::= class Identifier extends Identifier {implements Identifier}*
    ;;               {field Type Identifier}* {MethodDecl}*
    (class-decl ("class" identifier "extends" identifier (arbno "implements" identifier)
                         (arbno "field" type identifier) (arbno method-decl)) a-class-decl)
    
    ;; ClassDecl ::= interface Identifier {AbstractMethodDecl}*
    (class-decl ("interface" identifier (arbno abstract-method-decl)) an-interface-decl)
    
    ;; MethodDecl ::= method Type Identifier({Identifier : Type}*(,)) Expression
    (method-decl ("method" type identifier "(" (arbno identifier ":" type (arbno ",")) ")" expression) a-method-decl)
    
    ;; AbstractMethodDecl ::= method Type Identifier({Identifier : Type}*(,))
    (abstract-method-decl ("method" type identifier "(" (arbno identifier ":" type (arbno ","))")") an-abstract-method-decl)
    ;;======================================17:50 박승주 수정.============================
    ;; Expression ::= Number
    (expression (number) const-exp)
    
    ;; Expression ::= Identifier
    (expression (identifier) var-exp)
    
    ;; Expression ::= zero?(Expression)  
    (expression ("zero?" "(" expression ")") zero?-exp)
    
    ;; Expression ::= if Expression then Expression else Expression
    (expression ("if" expression "then" expression "else" expression) if-exp)
    
    ;; Expression ::= set Identifier = Expression
    (expression ("set" identifier "=" expression) assign-exp)
    
    ;; Expression ::= begin Expression {; Expression}* end
    (expression ("begin" expression (arbno ";" expression) "end") begin-exp)
    
    ;; Expression ::= +(Expression, Expression) 
    (expression ("+" "(" expression "," expression ")") sum-exp)
    
    ;; Expression ::= -(Expression, Expression)                  
    (expression ("-" "(" expression "," expression ")") diff-exp)
    
    ;; Expression ::= *(Expression, Expression)                                    
    (expression ("*" "(" expression "," expression ")") mul-exp)
    
    ;; Expression ::= /(Expression, Expression)                                    
    (expression ("/" "(" expression "," expression ")") div-exp)
    
    ;; Expression ::= %(Expression, Expression)
    (expression ("%" "(" expression "," expression ")") mod-exp)
    
    ;; Expression ::= list({Expression}*(,))                  
    (expression ("list" "("(arbno expression (arbno ",")) ")") list-exp)
    
    ;; Expression ::= let {Identifier = Expression}* in Expression  
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    
    ;; Expression ::= Expression({Expression}*(,))
    (expression (expression "(" (arbno expression (arbno ",")) ")") call-exp)
    
    ;; Expression ::= proc({Identifier : Type}*(,)) Expression
    (expression ("proc" "(" (arbno identifier ":" type (arbno ",")) ")" expression) proc-exp)
    
    ;; Expression ::= letrec{Type Identifier ({Identifier : Type}*(,)) = Expression}* in Expression  
    (expression ("letrec" (arbno type identifier "(" (arbno identifier ":" type (arbno ",")) ")" "=" 
                                 expression) "in" expression) letrec-exp)
    
    ;; Expression ::= new Identifier({Expression}*(,))
    (expression ("new" identifier "(" (arbno expression (arbno ",")) ")") new-object-exp)
    
    ;; Expression ::= send Expression Identifier({Expression}*(,))  
    (expression ("send" expression identifier "(" (arbno expression (arbno ",")) ")") method-call-exp)
    
    ;; Expression ::= self  
    (expression ("self") self-exp)
    
    ;; Expression ::== super Identifier ({Expression}*(,))
    (expression ("super" identifier "(" (arbno expression (arbno ",")) ")") super-call-exp)))

    ;; Expression ::= cast Expression Identifier
    (expression ("cast" identifier symbol) cast-exp)

    ;; Expression ::= instanceof Expression Identifier  
    (expression ("instanceof" expression symbol) instanceof-exp)

;; 5.14일 19:42분  parser-rule 까지 수정완료.(박승주)
    
    
(define scan&parse
  (sllgen:make-string-parser scanner-rules parser-rules))

;; ======================================================================

(define pgm->string
  (lambda (pgm)
    (cases program pgm
      (a-program (class-decls body)
        (format "~a~a"
              (letrec ((class-decls->string (lambda (class-decl-list)
                (cond
                  ((null? class-decl-list)
                    (format ""))
                  ((null? (cdr class-decl-list))
                    (format "~a" (class-decl->string (car class-decl-list))))
                  (else
                    (format "~a~a" (class-decl->string (car class-decl-list)) (class-decls->string (cdr class-decl-list))))))))
              (class-decls->string class-decls))
              (exp->string body)))
      (else
        (eopl:error 'pgm->string "arg=~a" pgm)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define class-decl->string
  (lambda (class-decl1)
    (cases class-decl class-decl1
      (a-class-decl (class-name super-name field-names method-decls)
        (format "class ~a extends ~a~a~a"
                (class-name)
                (super-name)
               (letrec ((field-names->string (lambda (var-list)
                 (cond
                   ((null? var-list)
                    (format ""))
                  ((null? (cdr var-list))
                    (format "~a" (exp->string (car var-list))))
                  (else
                    (format "~a~a" (exp->string (car var-list)) (field-names->string (cdr var-list))))))))
              (field-names->string field-names))

              (letrec ((method-decls->string (lambda (method-decl-list)
                (cond
                  ((null? method-decl-list)
                    (format ""))
                  ((null? (cdr method-decl-list))
                    (format "~a" (method-decl->string (car method-decl-list))))
                  (else
                    (format "~a~a" (method-decl->string (car method-decl-list)) (method-decls->string (cdr method-decl-list))))))))
              (method-decls->string method-decls)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define method-decl->string
  (lambda (method-decl1)
    (cases method-decl method-decl1
      (a-method-decl (method-name vars body)
        (format "method ~a(~a)~a"
              (method-name)
              (letrec ((vars->string (lambda (var-list)
                (cond
                  ((null? var-list)
                    (format ""))
                  ((null? (cdr var-list))
                    (format ",~a" (exp->string (car var-list))))
                  (else
                    (format "~a~a" (exp->string (car var-list)) (vars->string (cdr var-list))))))))
              (vars->string vars))
              (exp->string body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define list-var->string
  (lambda (list-var)
    (cond
      ((null? list-var)
         (format ""))
      ((null? (cdr list-var))
         (format "~a" (exp->string (car list-var))))
      (else
        (format "~a~a" (exp->string (car list-var)) (list-var->string (cdr list-var)))))))


(define exp->string
  (lambda (exp)
    (cases expression exp
      (const-exp (num)
        (format "~a" num))
      (var-exp (var)
        (format "~a" var))
      (zero?-exp (exp1)
        (format "zero?(~a)" (exp->string exp1)))
      (diff-exp (exp1 exp2)
	(format "-(~a,~a)" (exp->string exp1) (exp->string exp2)))
      (if-exp (exp1 exp2 exp3)
        (format "if ~a then ~a else ~a" (exp->string exp1) (exp->string exp2) (exp->string exp3)))
      (let-exp (vars exps body)
	;;; 용규 수정 ;;;
        ;;(format "let ~a=~a in ~a" var (exp->string exp1) (exp->string body)))
	(string-append "let " (let->exp vars exps) " in " (exp->string body)))
      (proc-exp (var body)
        (format "proc(~a)~a" var (exp->string body)))
      (call-exp (rator rand)
        (format "(~a ~a)" (exp->string rator) (exp->string rand)))
      (letrec-exp (p-names b-vars p-bodys letrec-body)
        (format "letrec ~a in ~a"
          (letrec ((proc-list->string (lambda (p-name-list b-var-list p-body-list)
                (cond
                  ((null? p-name-list)
                    (format ""))
                  ((null? (cdr p-name-list))
                    (format "~a(~a)=~a"
                       (car p-name-list) (car b-var-list) (exp->string (car p-body-list))))
                  (else
                    (format "~a(~a)=~a~a"
                       (car p-name-list) (car b-var-list) (exp->string (car p-body-list))
                       (proc-list->string (cdr p-name-list) (cdr b-var-list) (cdr p-body-list))))))))
            (proc-list->string p-names b-vars p-bodys))
          (exp->string letrec-body)))
      (begin-exp (exp1 exps)
        (format "begin ~a~a end" (exp->string exp1)
          (letrec ((exps->string (lambda (exp-list)
                (cond
                  ((null? exp-list)
                    (format ""))
                  ((null? (cdr exp-list))
		    (format ";~a" (exp->string (car exp-list))))
                  (else
		    (format ";~a~a" (exp->string (car exp-list)) (exps->string (cdr exp-list))))))))
              (exps->string exps))))
      (assign-exp (var exp1)
        (format "set ~a=~a" var (exp->string exp1)))
      (sum-exp (exp1 exp2)
        (format "+(~a,~a)" (exp->string exp1) (exp->string exp2)))
      (list-exp (lst)
	;;; 용규 수정 ;;;
	;;; (format "(~a)" ))
	(string-append "list " "(" (rands-expression lst) ")"))
;;class관련
      (new-object-exp (class-name rands)
        (string-append "new " (symbol->string class-name) "(" (rands-expression rands) ")"))
      (method-call-exp (obj-exp method-name rands)
        (string-append "send " (exp->string obj-exp) " " (symbol->string method-name) "(" (rands-expression rands) ")"))
      (self-exp ()
        (string-append "self"))
      (super-call-exp (method-name rands)
        (string-append "super " (symbol->string method-name) "(" (rands-expression rands) ")"))
      (else
        (eopl:error 'exp->string "arg=~a" exp)))))
;; ======================================================================

;; ({Expression}*) 구현하는 rands-expression
(define rands-expression
 (lambda (rands)
  (if (null? rands)
    ""
   (if (null? (cdr rands))
    (string-append (exp->string (car rands)))
    (string-append (exp->string (car rands)) " " (rands-expression (cdr rands)))))))

;; let {Identifier = Expression}* in Expression 에서 Identifier = Expression을 여러게 표현하기 위해 구현한 let->exp 함수

(define let->exp
 (lambda (vars exps)
  (if (null? vars)
   ""
   (if (null? (cdr vars))
    (string-append (symbol->string (car vars)) " = " (exp->string (car exps)))
    (string-append (symbol->string (car vars)) " = " (exp->string (car exps)) " " (let->exp (cdr vars) (cdr exps)))))))
