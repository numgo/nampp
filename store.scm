#lang eopl

(provide the-store empty-store get-store initialize-store! reference? newref deref setref! refs->string)
;; ----------------------------------------------------------------------

(define the-store 'uninitialized)

(define empty-store
  (lambda () '()))

(define get-store
  (lambda () the-store))

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define reference?
  (lambda (v)
    (integer? v)))

(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))

(define deref
  (lambda (ref)
    (list-ref the-store ref)))

(define setref!
  (lambda (ref val)
    (set! the-store
      (letrec ((setref-inner (lambda (store1 ref1)
              (cond ((null? store1)
                  ;(report-invalid-reference ref the-store))
                  (eopl:error "report-invalid-reference"))
                ((zero? ref1)
                  (cons val (cdr store1)))
                (else
                  (cons (car store1)
                    (setref-inner (cdr store1) (- ref1 1))))))))
        (setref-inner the-store ref)))))

;; list-of references 들을 string으로 변환하는 procedure
(define refs->string
 (lambda (refs)
  (if (null? refs)
   ""
   (if (null? (cdr refs))
    (string-append (number->string (car refs)))
    (string-append (number->string (car refs)) " " (refs->string (cdr refs)))))))

;; ----------------------------------------------------------------------
