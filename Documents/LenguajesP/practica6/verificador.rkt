#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))

;; Toma un Árbol de sintaxis abstracta CFWBAE y obtiene el tipo
;; de la expresión mínima.
;; typeof CFWBAE -> Type-Context -> Type
;; (define (typeof expr context)
(define (typeof expr context)
  (type-case SCFWBAE expr
      [idS (i) (lookupType exp context)]
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
                    (error 'typeof "Los argumentos para ~a deben son de tipo number" f))]
            [(member f (list = < <= > >= zero?))
               (if (for/and ([i args]) (numberT? (typeof i context)))
                    (booleanT)
                    (error 'typeof "Los argumentos para ~a deben son de tipo number" f))]
            [(member f (list not myAnd myOr))
               (if (for/and ([i args]) (booleanT? (typeof i context)))
                    (booleanT)
                    (error 'typeof "Los argumentos para ~a deben son de tipo boolean" f))]
            )]

            [condS (cases)
             (let ([rtype (condType (car cases) context)])
               (if (for/and([i (cdr cases)]) (equal? rtype (condType i context)))
                   rtype
                   (error 'typeof "Los tipos de retorno en ~a son distintos" expr)))]
      ;[withS]
      ;[withS*]
      [funS (params rType body)
             (let ([typep (for/list ([i params]) (param-tipo i))])
               (begin
                 (set! context (newContext params context)) ;;Tambien hubiera funcionado creo que con for/fold pero no se manejarlo 
                  (funT (append typep (list rType)))
                 ))]
      ;[appS (fun args)
       ;    (interp (fun-body fun) (newEnv ds (fun-params fun) args))]
      [else 1]))
  
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
