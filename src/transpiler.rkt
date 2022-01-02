#lang errortrace racket
(require "transpiler-util.rkt")

(provide (all-defined-out))

;; Utility function that converts a variable into a string
;; Useful when translating from SimpleJS into LambdaJS
(define (mk-field x)
  (match x [(s:variable x) (k:string (symbol->string x))]))

;; Utility function that allocates a j:object.
;; (mk-object) allocates an empty object
;; (mk-object (cons "foo" (k:number 1)) (cons "bar" (j:variable 'x)))
;;  allocates an object with one field "foo" and one field "bar"
(define/contract (mk-object . args)
  (-> (cons/c string? j:expression?) ... j:alloc?)
  (define (on-elem pair)
    (cons (k:string (car pair)) (cdr pair)))
  (j:alloc (j:object (make-immutable-hash (map on-elem args)))))


;; Translation function from SimpleJS to LambdaJS
(define/contract (translate exp)
  (-> s:expression? j:expression?)
  (match exp
    [(? k:const? k) k]
    [(s:variable x) (j:variable x)]
    [(s:let (s:variable x) s1 s2)
     (j:let (j:variable x) (translate s1) (translate s2))]
    [(s:apply f ea) (j:apply (translate f) (map translate ea))]
    ((s:new id params)
     (mk-let
      (j:deref (translate id))
      (lambda (ctor)
        (let ((fields (cons "$proto" (j:get ctor (k:string "prototype")))))
        (mk-let
          (mk-object fields)
          ;f(obj, J[e]...)
          (lambda (obj)
            (let ((in-f (cons obj (map translate params))))
            (j:seq
             (j:apply (j:get ctor (k:string "$code")) in-f)
             obj
             )
            )
         ))
        ))))
    ((s:invoke obj method args)
     (let ((j-obj (j:deref (j:get (j:deref (translate obj)) (mk-field method))))) 
       (j:apply (j:get j-obj (k:string "$code")) (cons (translate obj) (map translate args))
    )))
    ((s:function params body)
     (let ((lam-this (j:lambda (cons (j:variable 'this) (map translate params)) (translate body))))
       ;{"$code" : λ(this,x···).[[e]],"prototype" : alloc {}} 
       (mk-object (cons "$code" lam-this) (cons "prototype" (mk-object)))
       )
     )
    ((s:load obj field)
     (let ((j-obj (j:deref (translate obj))))
       (j:get j-obj (mk-field field))
    ))
    ((s:assign obj field arg)
     (mk-let
      (translate arg)
      (lambda (rhs)
        (j:seq (j:assign (translate obj) (j:set (j:deref (translate obj)) (mk-field field) rhs)) rhs)
        )
      ))
    ))

