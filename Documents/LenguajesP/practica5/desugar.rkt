#lang plai
(require (file "./grammars.rkt"))
(require (file "./parser.rkt"))


;; Función que toma una expresión con azúcar sintáctica
;; SCFWBAE y elimina el azúcar sintáctica, tansformándola
;; en una expresión del tipo CFWBAE; formando el árbol de
;; sintáxis abstracta correspondiente a la expresión recibida.
;; desugar SCFWBAE-> CFWBAE
;; (define (desugar sexpr))
(define (desugar sexpr)
  (cond
    [(idS? sexpr) (id (idS-i sexpr))]
    [(numS? sexpr) (num (numS-n sexpr))]
    [(boolS? sexpr) (bool (boolS-b sexpr))]
    [(iFS? sexpr) (iF (desugar (iFS-condicion sexpr)) (desugar(iFS-then sexpr)) (desugar (iFS-else sexpr)))]
    [(opS? sexpr) (op (opS-f sexpr) (map desugar (opS-args sexpr)))]
    [(withS? sexpr) (app
                        (fun (for/list ([ j (withS-bindings sexpr)])
                                (binding-id j))
                             (desugar (withS-body sexpr)))
                        (for/list  ([l (withS-bindings sexpr)]) (desugar (binding-value l))))]

    [(withS*? sexpr) (desugar (withappends sexpr))]
    [(funS? sexpr) (fun (funS-params sexpr) (desugar (funS-body sexpr)))]
    [(appS? sexpr) (app (desugar (appS-fun sexpr)) (map desugar (appS-args sexpr)))]
    ;;cond
    [(condS? sexpr)
      (let ([cond0 (car (condS-cases sexpr))] [condtl (cdr (condS-cases sexpr))])
        (cond
             [(else-cond? cond0) (desugar (else-cond-else-expr cond0))]
             [(condition? cond0)
              (iF (desugar (condition-test-expr cond0))
                  (desugar (condition-then-expr cond0))
                  (if (empty? condtl)
                      (error 'desugar "Error de sintaxis: No existe caso de término en el condicional")
                      (desugar (condS condtl)))
              )]
             ))]))


;;Función auxiliar que me transforma with* en with anidados
;;Sexp->Sexp
(define (withappends wS)
  (cond
    [(withS*? wS)
     (let ([binds (withS*-bindings wS) ] [body (withS*-body wS)])
                    (cond
                      [(empty? binds) body]
                      [else (withS (list (car binds)) (withappends (withS* (cdr binds) body)))]))]
    [else  error 'withappends "Error: withappends no recibo un withS*"]))