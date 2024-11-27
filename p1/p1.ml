(*Victor Nathanael Badillo Aldama*)
(*P1*)



(*1.-Implemente una función es_afne : Auto.af -> bool que reciba como argumento un autómata
finito, y que devuelva true si se trata de un autómata que presenta alguna épsilon-transición, o false en
caso contrario*)



let rec es_afne (Af (_, _, _, arcos, _)) =
   let rec aux = function 
     [] -> false 
   | Arco_af (_, _, Terminal "") :: _  -> true
   | _ :: rest -> aux rest  
   in aux (list_of_conjunto arcos);;
   
  
  
(*Implemente una función es_afn : Auto.af -> bool que reciba como argumento un autómata
finito, y que devuelva true si se trata de un autómata que presenta algún tipo de no determinismo
(excepto épsilon-transiciones), o false en caso contrario.*)



let es_afn (Af (estados, alfabeto, _, conj_arcos, _))=
  let rec arcos_repetidos = function
   [] -> false
   | Arco_af (estado_origen, _, simbolo) :: rest ->
     List.exists (fun (Arco_af (origen, _, s)) ->
       estado_origen = origen && s = simbolo
      ) rest || arcos_repetidos rest
  in
  let faltan_arcos estados alfabeto conj_arcos =
     let check = list_of_conjunto (cartesiano estados alfabeto) in
     let arcos = conjunto_of_list (List.map (fun (Arco_af (estado, _, simbolo)) -> (estado,simbolo)) (list_of_conjunto conj_arcos)) in
     let rec aux = function
      [] -> false
     | x :: rest -> if pertenece x arcos then aux rest 
                  else true
     in aux check
  in
  arcos_repetidos (list_of_conjunto conj_arcos) || faltan_arcos estados alfabeto conj_arcos;;



(*Implemente una función es_afd : Auto.af -> bool que reciba como argumento un autómata
finito, y que devuelva true si se trata de un autómata totalmente determinista, o false en caso contrario.*)



let es_afd af = 
  not (es_afne af || es_afn af);;



(*2.- Implemente una función equivalentes : Auto.af -> Auto.af -> bool que reciba como
argumentos dos autómatas finitos y que devuelva true cuando ambos autómatas acepten el mismo
lenguaje, o false en caso contrario.*)



let equivalentes (Af(_, simb1, inicial1, arcos1, finales1) as af1) (Af(_,simb2, inicial2, arcos2, finales2) as af2) =
  let simbolos = list_of_conjunto (union simb1 simb2) in 
  let rec aux pendientes visitados = match pendientes with
     [] -> true
   | (estados1, estados2) :: rest -> 
      if pertenece (estados1,estados2) visitados then
         aux rest visitados
      else begin
        let visitados' = agregar (estados1, estados2) visitados in
        let term1 = interseccion estados1 finales1 in
        let term2 = interseccion estados2 finales2 in
        if ((not (es_vacio term1)) && (es_vacio term2)) || ((not (es_vacio term2)) && (es_vacio term1)) then 
          false
        else begin
          let nuevos_pendientes = List.map (fun simbolo ->
            let nuevos_estados_af1 = epsilon_cierre (avanza simbolo estados1 af1) af1 in
            let nuevos_estados_af2 = epsilon_cierre (avanza simbolo estados2 af2) af2 in
            (nuevos_estados_af1, nuevos_estados_af2)
          ) simbolos in
          
          let nueva_cola_pendientes = rest @ nuevos_pendientes in
          aux nueva_cola_pendientes visitados'
        end    
      end
  in aux [( epsilon_cierre (Conjunto [inicial1]) af1, epsilon_cierre (Conjunto [inicial2]) af2)] (Conjunto []);;



(*3.- [Ejercicio opcional] La librería ocaml_talf proporciona la función escaner_af :
Auto.simbolo list -> Auto.af -> bool, que dada una lista de símbolos terminales y un
autómata finito indica si dicha cadena de símbolos es aceptada o no por el autómata. Se trata de una versión
de la función de reconocimiento más general posible, es decir, aquélla que es capaz de simular el
funcionamiento de cualquier tipo de autómata finito (determinista, no determinista, e incluso no
determinista con épsilon-transiciones).*)



(*Reimplemente la función anterior para definir ahora una función escaner_afn : Auto.simbolo
list -> Auto.af -> bool, que dada una lista de símbolos terminales y un autómata finito indica si
dicha cadena de símbolos es aceptada o no por el autómata. Se trata de una versión de la función de
reconocimiento que es capaz de simular de manera más óptima el funcionamiento de autómatas finitos con
cualquier tipo de no determinismo (excepto épsilon-transiciones)*)


let escaner_afn cadena (Af (_, _, inicial, _, finales) as a) =

   let rec aux = function

        (Conjunto [], _) ->
           false

      | (actuales, []) ->
           not (es_vacio (interseccion actuales finales))

      | (actuales, simbolo :: t) ->
           aux ((avanza simbolo actuales a), t)

   in
      aux ((Conjunto [inicial]), cadena)
   ;;



(*Reimplemente la función anterior para definir ahora una función escaner_afd : Auto.simbolo
list -> Auto.af -> bool, que dada una lista de símbolos terminales y un autómata finito indica si
dicha cadena de símbolos es aceptada o no por el autómata. Se trata de una versión de la función de
reconocimiento que es capaz de simular de manera todavía más óptima el funcionamiento de autómatas
finitos totalmente deterministas*)



let escaner_afd cadena (Af (_, _, inicial, conj_arcos, finales) as a) =
  let arcos = list_of_conjunto conj_arcos in
  try
    let rec aux estado_actual cadena = match cadena with
    [] -> incluido (Conjunto [estado_actual]) finales
    | x :: rest -> let (Arco_af(ori,dest,simb)) = List.find (fun (Arco_af(origen, destino, simbolo)) -> origen = estado_actual && simbolo = x ) arcos 
       in aux dest rest
    in aux inicial cadena
   with
   | Not_found -> false
;;













