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
    [iF (condicion then else) (error "Sin declararse")]
    [op (f args)
        (if ((lambda (l) (for/or  ([i (list + - * / modulo expt add1 sub1 <  <=  > >= equal?  zero?)])(equal? l i )))f)
            ;;Condición para procedures
            (cond
              [(for/or ([i (list + - * / modulo expt add1 sub1 <  <= > >= )]) (equal? f i))
               (cond
                 [(for/and ( [i args]) (numV? (interp i ds)))
                   (cond 
                       [(and (equal?(length args) 1) (or (equal? f add1) (equal? f sub1)))
                        (numV (apply f (numV-n (interp (car args) ds)))) ]

                       [(and (equal? (length args) 2)(equal? f modulo) (equal? f expt))
                        (numV (apply f (numV-n (interp (args) ds))))]

                       [else
                        (cond
                          [(or (equal? f +) (equal? f -) (equal? f *) (equal? f /) )
                           (numV (apply f (for/list ([i args]) (numV-n (interp i ds)))))]
                          [else  (boolV (apply f (for/list ([i args]) (numV-n (interp i ds)))))])
                        ]
                        
                       )]
                  [else (error "Alguno de los argumentos no es un número")]
                  )]
              [ (equal? f equal?)
                 (if (equal? (length args) 2)
                   (boolV (apply f ( list (interp (first args) ds) (interp (second args) ds))))
                    (error "Se requieren 2 elementos para ="))]

              [(equal? f zero?)
               (if (equal? (length args) 1)
                   (if (numV? (interp (car args) ds))
                              (boolV (f (car args)))
                              (error "Error se espera un número"))
                   (error "Se requiere un elemento para zero?"))
               ])
            ;;Condición para no procedures
            (cond
              [(for/and ([j args]) (boolV? (interp j ds)))
               (boolV (f ((lambda(l)
                 (for/list ([bol l]) ( boolV-b  (interp bol ds) ) )) args)))]
              [else (error "Alguno de los argumentos no es un booleano")])
            )]
    [fun  (params body) (error "Sin declararse")]
    [app (fun args) (error "Sin declararse")]
    ))