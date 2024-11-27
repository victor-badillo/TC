(*
- Victor Nathanael Badillo Aldama
- Teoría da computación
- Práctica 0
*)

(*Ejercicio 1*)

(*
Implemente la función mapdoble.
*)

let mapdoble f1 f2 l = 
   let rec aux acc = function
   [] -> List.rev acc
   | h1::h2::t -> aux ((f2 h2) :: (f1 h1) :: acc) t
   | h::t -> aux ((f1 h) :: acc ) t
   in aux [] l;;
   
(*
Indique el tipo de la funcion mapdoble
val mapdoble : ('a -> 'b) -> ('a -> 'b) -> 'a list -> 'b list = <fun>

Indique el valor de: mapdoble (function x -> x*2) (function x -> "x") [1;2;3;4;5];;
Error: This expression has type string but an expression was expected of type int

Indique el tipo de: let y = function x -> 5 in mapdoble y;;
- : ('_weak1 -> int) -> '_weak1 list -> int list = <fun>
*)

(*Ejercicio 2*)

let rec primero_que_cumple f  = function
  [] -> raise Not_found
  | h::t -> if f h then h else primero_que_cumple f t;;
  
(*
Indique el tipo de la función primero_que_cumple.
val primero_que_cumple : ('a -> bool) -> 'a list -> 'a = <fun>
*)

(*
Utilizando la función primero_que_cumple, defina una función existe, que dado un
predicado y una lista devuelva true si en la lista hay algún elemento que verifica el
predicado, y false en caso contrario.
*)

let existe f l = 
   try 
      let _ = primero_que_cumple f l in true;
   with 
   | Not_found -> false;;
   
(*
val existe : ('a -> bool) -> 'a list -> bool = <fun>
*)

(*
Se quiere mantener un conjunto de valores etiquetados de cualquier tipo, mediante una lista
de pares 'a * 'b, donde la primera componente del par es la etiqueta o clave, y la
segunda es el valor asociado a esa clave en dicho conjunto. Utilizando la función
primero_que_cumple, defina una función asociado : ('a * 'b) list -> 'a -> 'b,
que dado un conjunto y una clave, devuelva su valor asociado.
*)

let asociado l key = 
     let value = primero_que_cumple (function x -> (fst x) = key) l in snd value;;
     
(*
val asociado : ('a * 'b) list -> 'a -> 'b = <fun>
*)

(*Ejercicio 3*)

type 'a arbol_binario =
Vacio
| Nodo of 'a * 'a arbol_binario * 'a arbol_binario;;

(*
Construya las funciones in_orden, pre_orden, post_orden y anchura, todas ellas de tipo
'a arbol_binario -> 'a list, que devuelvan los correspondientes recorridos sobre un
árbol binario dado, tal y como se muestra en los siguientes ejemplos:
*)

let t = Nodo(3, Nodo(2, Vacio, Vacio), Nodo(5,Nodo(4, Vacio, Vacio) , Nodo(1, Vacio, Vacio)));;

let rec in_orden = function
     Vacio -> []
    | Nodo (r,lb,rb) -> in_orden lb @ [r] @ in_orden rb;;
    
let rec pre_orden = function 
   Vacio -> []
  | Nodo (r, lb, rb) -> [r] @ pre_orden lb @ pre_orden rb;;
  
let rec post_orden = function
   Vacio -> []
   | Nodo (r, lb, rb) -> post_orden lb @ post_orden rb @ [r];;
   
let rec anchura arbol =
  let rec aux nodos resultado = match nodos with
    | [] -> resultado
    | Nodo(r, lb, rb) :: rest ->
      let nodes = rest @ [lb; rb] in
      aux nodes (resultado @ [r])
    | Vacio :: rest -> aux rest resultado
  in
  aux [arbol] [];;

in_orden t;;

pre_orden t;;

post_orden t;;

anchura t;;

(*Ejercicio 4*)

type 'a conjunto = Conjunto of 'a list;;


(*
• pertenece : 'a -> 'a conjunto -> bool
• agregar : 'a -> 'a conjunto -> 'a conjunto
• conjunto_of_list : 'a list -> 'a conjunto
• suprimir : 'a -> 'a conjunto -> 'a conjunto
• cardinal : 'a conjunto -> int
• union : 'a conjunto -> 'a conjunto -> 'a conjunto
• interseccion : 'a conjunto -> 'a conjunto -> 'a conjunto
• diferencia : 'a conjunto -> 'a conjunto -> 'a conjunto
• incluido : 'a conjunto -> 'a conjunto -> bool
• igual : 'a conjunto -> 'a conjunto -> bool
• producto_cartesiano : 'a conjunto -> 'b conjunto -> ('a * 'b) conjunto
• list_of_conjunto : 'a conjunto -> 'a list
*)

let rec pertenece x = function 
   Conjunto [] -> false
   | Conjunto (h::t) -> if x = h then true else pertenece x (Conjunto t);;
   
let agregar x conj = 
   if pertenece x conj then 
      raise(Failure"No se puede agregar un elemento existente al conjunto")
   else 
      match conj with 
      | Conjunto l -> Conjunto (x::l);;  
   
let conjunto_of_list l =
   let rec aux l acc = match l with 
   [] -> Conjunto (acc)
   | h::t -> if (pertenece h (Conjunto acc)) = false then aux t (h::acc) else aux t acc
   in aux l [];;
   
let suprimir y = function
   Conjunto [] -> Conjunto []
   | Conjunto l -> conjunto_of_list(List.filter (fun x -> x <> y) l);;
   
let cardinal (Conjunto l) = List.length l;;  
   
let union (Conjunto l1) (Conjunto l2) = conjunto_of_list (List.concat [l1; l2]);;
   
let interseccion (Conjunto l1) conj2 =
   let rec aux l1 conj2 acc = match l1 with
   [] -> Conjunto (acc)
   | h::t -> if (pertenece h conj2) = true then aux t conj2 (h::acc) else aux t conj2 acc
   in aux l1 conj2 [];;  
   
let diferencia (Conjunto lst1) conj2 =
  let lista_diferencia = List.filter (fun x -> not (pertenece x conj2)) lst1 in
  Conjunto lista_diferencia;;
  
let incluido conj1 conj2 = let result = (diferencia conj1 conj2) in  result =  (Conjunto []);;
   
let igual conj1 conj2 = (incluido conj1 conj2) && (incluido conj2 conj1);;

let producto_cartesiano (Conjunto l1) (Conjunto l2)= Conjunto (List.flatten(List.map( function x -> List.map(function y ->(x,y)) l2) l1));;

let list_of_conjunto (Conjunto l) = l;;  
   
   
   
   
   
   
   
   
   
   
   
   
   




























