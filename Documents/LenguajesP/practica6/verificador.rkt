#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Toma un Árbol de sintaxis abstracta CFWBAE y obtiene el tipo
;; de la expresión mínima.
;; typeof CFWBAE -> Type-Context -> Type
;; (define (typeof expr context)
(define (typeof expr context)
  (type-case SCFWBAE expr
      [idS (i) (lookupType i context)]
      [numS (n) (numberT)]
      [boolS (b) (booleanT)]
      [iFS (condition thenE elseE)
          (let ([conditionType (typeof condition context) ])
                (if (booleanT? conditionType)
                    (let ([typeThen (typeof thenE context)] [typeElse (typeof elseE context)])
                        (if (equal? typeThen typeElse)
                            typeThen
                            (error 'typeof "Los tipos de retorno en ~a son distintos" expr)))
                (error 'typeof "Error en la condicion ~a , no es de tipo boolean" condition)))]
      [opS (f args)
          (cond
            [(member f (list + - * / modulo expt add1 sub1))
                (if (for/and ([i args]) (numberT? (typeof i context)))
                    (numberT)
                    (error 'typeof "Los argumentos para ~a deben ser de tipo number" f))]
            [(member f (list = < <= > >= zero?))
               (if (for/and ([i args]) (numberT? (typeof i context)))
                    (booleanT)
                    (error 'typeof "Los argumentos para ~a deben ser de tipo number" f))]
            [(member f (list not myAnd myOr))
               (if (for/and ([i args]) (booleanT? (typeof i context)))
                    (booleanT)
                    (error 'typeof "Los argumentos para ~a deben ser de tipo boolean" f))]
            )]

      [condS (cases)
            (let ([rtype (condType (car cases) context)])
                (if (and
                        (for/and ([i (cdr cases)]) (equal? rtype (condType i context)))
                        (for/and ([i cases]) ((lambda (x) (type-case Condition x
                            [condition (cnd-e then-e) (equal? (booleanT) (typeof cnd-e context))]
                            [else-cond (else-e) #t]) ) i) )
                    )
                    rtype
                   (error 'typeof "Los tipos de retorno en ~a son distintos" expr)))]
      [withS (bdins body)
            (let* ([ids (for/list ([i bdins]) (binding-id i))]
                    [types (for/list ([i bdins]) (binding-tipo i))]
                    [values (for/list ([i bdins]) (binding-value i))]
                    [newContext (for/fold
                                ([cum context])
                                ([i ids] [j types] [val values])
                                (if (equal? j (typeof val cum))
                                    (gamma i j cum)
                                    (error 'typeof (string-append "El id " (~a i) " tiene declarado tipo " (~a j)
                                                        " pero su value es de tipo " (~a (typeof val cum))))) )])
                    (typeof body newContext) )]
      [withS* (bdins body)
                  (let* ([ids (for/list ([i bdins]) (binding-id i))]
                          [types (for/list ([i bdins]) (binding-tipo i))]
                          [values (for/list ([i bdins]) (binding-value i))]
                          [newContext (for/fold
                                      ([cum context])
                                      ([i ids] [j types] [val values])
                                      (if (equal? j (typeof val cum))
                                          (gamma i j cum)
                                          (error 'typeof (string-append "El id " (~a i) " tiene declarado tipo " (~a j)
                                                              " pero su value es de tipo " (~a (typeof val cum))))) )])
                          (typeof body newContext) )]
      [funS (params rType body)
             (let* ([typep (for/list ([i params]) (param-tipo i))]
                    [ids (for/list ([j params]) (param-param j))]
                    [newContext (for/fold ([cum context])
                                          ([i ids]  [j typep])
                                          (gamma i j cum))]
                    [typeBody (typeof body newContext)])
                    (if (equal? rType typeBody)
                        (funT (append typep (list rType)))
                        (error 'typeof (string-append "El tipo de retorno declarado para la funcion es "  (~a rType) " pero el del body es " (~a typeBody)))) )]
      [appS (fun args)
            (let* ([funType (if (funT? (typeof fun context)) (typeof fun context) (error "La funcion ~a no es de tipo funT" fun))]
                    [argsTypes (for/list ([i args]) (typeof i context))] )
                    (if (for/and ([i (funT-params funType)] [j argsTypes]) (equal? i j))
                        (last (funT-params funType))
                        (error 'typeof (string-append "Los tipos de los parametros de " (~a fun) " difieren con los de " (~a args)))) )]  ))
  
(define (prueba exp)
  (typeof (parse exp) (phi)))


;;Funcion para buscar tipos en un contexto
;; lookupType: symbol -> Type-Context -> Type
(define (lookupType name ds)
  (type-case Type-Context ds
    [phi () (error 'lookupType "Hay un identificador sin tipo definido: ~a" name)]
    [gamma (nameT val dss)
      (if (equal? name nameT) val
          (lookupType name dss))]))


;;Funcion que me extraera el tipo de una conditional
;; condType: condition -> Type-Context -> Type 
(define (condType c context)
  (type-case Condition c
     [condition (test-expr then-expr)
                (if (booleanT? (typeof  (condition-test-expr c) context))
                   (typeof  (condition-then-expr c) context)
                   (error 'condition "El test-expr debe de ser tipo booleanT"))
                 ]
  [else-cond  (else-expr)
              (typeof (else-cond-else-expr c) context)]))
