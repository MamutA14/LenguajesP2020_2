#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))
(require (file "./desugar.rkt"))

;; Busca el identificador "name" en el caché de 
;; sustitución "ds" regresando el valor correspondiente
;; o informando un error si no lo encuentra.
;; lookup: symbol DefrdSub -> CFWBAE
;; (define (lookup name ds)
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "Hay un identificador libre: ~a" name)]
    [aSub (nameT val dss)
      (if (equal? name nameT) val
          (lookup name dss))]))


;; Toma un árbol de sintáxis abstraca del lenguaje CFWBAE, un caché de
;; sustituciones y lo interpreta dependiendo de las definiciones dentro del caché,
;; devolviendo el valor numérico correspondiente.
;; interp: CFWBAE DefrdSub-> CFWBAE-Value
  (define (interp expr ds)
    (type-case CFWBAE expr
    [id (i) (interp (lookup i ds) ds )]
    [num (n) (numV n)]
    [bool (b) (boolV b)]
    [iF (condicion then else)
        (let ([contion (interp condicion ds) ])
          (if (boolV? contion)
            (if (boolV-b  contion)
                (interp then ds)
                (interp else ds))
             (error 'interp "Error la condicional no puede ser evaluada ")))]
    [op (f args)
        (cond
            [(or (equal? f myAnd) (equal? f myOr))
                (if (for/and ([i args]) (bool? i))
                    (boolV (f (map (lambda (x) (cond [(numV? x) (numV-n x)] [(boolV? x) (boolV-b x)] )) (for/list ([i args]) (interp i ds)))))
                    (error 'interp "Type error: la funcion ~a debe aplicarse con argumentos de tipo boolean" f) )]
            [(not (procedure-arity-includes? f (length args))) (error 'interp "Error de aridad al aplicar ~a" f)]
            [(equal? f not)
                (if (bool? (car args)) (apply f (boolV-b (interp (car args) (mtSub))))
                    (error 'interp "Type error: la funcion not debe aplicarse con un boolean"))]
            [(equal? f zero?)
                (if (num? (car args)) (f (numV-n (interp (car args) (mtSub))))
                                    (error 'interp "Type error: la funcion zero? debe aplicarse con un number")) ]
            [else
                (if (for/and ([i args]) (num? i))
                    (numV (apply f (map (lambda (x) (cond [(numV? x) (numV-n x)] [(boolV? x) (boolV-b x)] )) (for/list ([i args]) (interp i ds)))))
                    (error 'interp "Type error: la funcion ~a debe aplicarse con argumentos del tipo number" f) )]  )]
    [fun  (params body) (closure  params  body  ds)]
    [app (fun args)
         (interp (fun-body fun) (newEnv ds (fun-params fun) args))] ))

;;Función auxiliar que me insertara nuevos valores al ambiente
;; newAnv:  DefrdSub ->listof symbol -> listof CFWAE -> DefrdSub
(define (newEnv ds params args)
  (if (and (empty? params) (empty? args))
      ds
      (if (equal? (length params) (length args))
          (newEnv (aSub (car params) (car args) ds) (cdr params) (cdr args))
          (error 'newEnv "No hay una relación biyectiva entre params y args"))))