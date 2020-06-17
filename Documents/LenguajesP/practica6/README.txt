Nombre:                     Num de cuenta:      correo:
Acosta Meza Alam            315124569           alam@ciencias.unam.mx
Sierra Casiano Vladimir     316020361           vladimirsierra@ciencias.unam.mx



Para evitar confusion con la sintaxis de las s-expresion considere los siguientes ejemplos:

(prueba '{fun {{x : number} {y : boolean}}: number {+ x y} })
(prueba '{with {(x : number 2)} x})
(prueba '{{fun {{x : number}}: boolean {< 0 x}} {2}})
(prueba '{with* {(x : number 2) (y : boolean #t )} {and y #f}})
(prueba '{{fun {(x : number) (y : boolean)}: number {if y {+ x 3} {- x 2}}} {4 #f}})
(prueba '{cond {{< 0 1} 3} {#t 4} {{= 1 2} {+ 2 5}} {else 6} })

