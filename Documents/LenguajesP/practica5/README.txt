Nombre:                     Num de cuenta:      correo:
Acosta Meza Alam            315124569           alam@ciencias.unam.mx
Sierra Casiano Vladimir     316020361           vladimirsierra@ciencias.unam.mx



Observacio: en la sintaxis abstracta el simbolo = representa el procedure homónimo en Racket. No confundir
            con 'equal?', la funcion '=' solo compara argumentos de tipo number.



Para evitar confusiones con la sintaxis abstracta considere los siguientes ejemplos de entrada:

(interp (desugar (parse '{cond {{< 3 1} 1} {#f 2} {else 5}})) (mtSub))
(interp (desugar (parse '{>= 2 {if #t 1 3}})) (mtSub))
(interp (desugar (parse '{with* {{x 2} {y {expt x 2}}} {if {= x 2} {modulo y 3} 7 }})) (mtSub))
(interp (desugar (parse '{{fun {x y} {= x y}} {7 4}})) (mtSub))

