#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta CFWBAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> SCFWBAE
;; parse: s-expression -> SCFWBAE
(define (parse sexp)
  (cond
        [(symbol? sexp) (idS sexp)]
        [(number? sexp)  (numS sexp)]
        [(boolean? sexp) (boolS sexp)]
        [(list? sexp)
         (case (first sexp)
                [(+)  (opS +  (map parse (cdr sexp)))]
                [(-)  (opS - (map parse (cdr sexp)))]
                [(*)  (opS * (map parse (cdr sexp)))]
                [(/)  (opS / (map parse (cdr sexp)))]
                [(modulo) (opS modulo (map parse (cdr sexp)))]
                [(expt)  (opS expt (map parse (cdr sexp)))]
                [(add1)  (opS add1 (map parse (cdr sexp)))]
                [(sub1)  (opS sub1 (map parse (cdr sexp)))]
                [(<)  (opS <  (map parse (cdr sexp)))]
                [(<=)  (opS <=  (map parse (cdr sexp)))]
                [(>)  (opS >  (map parse (cdr sexp)))]
                [(>=)  (opS >=  (map parse (cdr sexp)))]
                [(=) (opS = (map parse (cdr sexp)) )]
                [(not)  (opS not  (map parse (cdr sexp)) )]
                [(and)  (opS myAnd (map parse (cdr sexp)))];;"and" y "or" no son procedures por ende se declaro de está manera
                [(or)  (opS myOr (map parse (cdr sexp)))]
                [(zero?)  (opS zero?  (map parse (cdr sexp)))]
                [(cond)
                 (if (accuelse (cdr sexp))
                     (condS (for/list ([i (cdr sexp)])
                          (if (equal? (length i) 2)
                           (case (first i) 
                             [(else) (else-cond (parse (second i)))]
                             [else (condition (parse (first i)) (parse (second i))) ])
                           (error 'parse "Error de sintaxis: Aridad incorrecta de algún argumento"))))
                     (error 'parse "Error de sintaxis: No puede existir otra condición despues del else"))]
           
                [(if) (let ([sexp0 (cdr sexp)])
                          (if (equal? 3 (length sexp0))
                                (iFS  (parse  (first sexp0)) (parse  (second sexp0)) (parse  (third sexp0)))
                                (error 'parse "Error de sintaxis: Aridad incorrecta para funcion iFS")))]
                [ (with)
                            (let ([sexp0 (cdr sexp)])
                                      (if (equal? (length sexp0) 2)
                                       (withS (for/list ([ j (first sexp0)])
                                                 (cond
                                                   [(and (equal? 2 (length j)) (symbol? (car j))) (binding (first j) (parse (second j))) ]
                                                   [else (error 'parse "Error de sintaxis: Asignacion de valores no valida en withS.")] ))
                                                   (parse (second sexp0))) 
                                       (error 'parse "Error de sintaxis: Aridad incorrecta  en withS")))]

               [ (with*)
                             (let ([sexp0 (cdr sexp)])
                                      (if (equal? (length sexp0) 2)
                                       (withS* (for/list ([ j (first sexp0)])
                                                 (cond
                                                   [(and (equal? 2 (length j)) (symbol? (car j))) (binding (first j) (parse (second j))) ]
                                                   [else (error 'parse "Error de sintaxis: Asignacion de valores no valida en withS*.")] ))
                                                   (parse (second sexp0))) 
                                       (error 'parse "Error de sintaxis: Aridad incorrecta  en withS*")))
                             ]
              [(fun)
               (let ([sexp0 (cdr sexp)])
                        (if (equal? (length sexp0) 2)
                          (funS (for/list ([l (first sexp0)])
                                (if (symbol? l) l
                                (error 'parse "Error de sintaxis: Error al definir la lista de ids en funS")))
                             (parse (second sexp0)))
                        (error 'parse "Error de sintaxis: Aridad incorrecta  en funS*")) ) ]

           [else
                (if (= (length sexp) 2)
                    (appS (parse (first sexp)) (for/list ([j (second sexp)]) (parse j)))
                (error "Error de verificación: Procedure no declarado"))])  ]
       [else  (error "Error de verificación: s-expresion no valida")]  ))

;;Función que servira para verificar que haya en cond
;;un solo else y además sea la última condición
;; list-> boolean
(define (accuelse sexp)
  (cond
    [(empty? sexp ) #t] ;; Si encuentra un unico else o no se encuentra ninguno
    [else (cond
            [(equal? 'else (caar sexp))
             (if (not (empty? (cdr sexp)))
                 #f
                 (accuelse (cdr sexp)))]
            [else (accuelse (cdr sexp))] )]))


;;Funcion que regresa el and de los elementos de una lista
;;list(boolan) -> boolean
(define (myAnd l)
         (cond
            [(list? l) (andmap (lambda(x) (and x)) l)]
            [else (and l)]))


;;Funcion que regresa el or de los elementos de una lista
;;list(boolan) -> boolean
(define (myOr l)
            (cond
                [(list? l) (ormap (lambda(x) (or x )) l)]
                [else (or l)] ))