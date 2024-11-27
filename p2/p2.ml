(*Víctor Nathanael Badillo Aldama*)
(*Práctica 2*)

(*Implemente una función es fnc : Auto.gic -> bool que indique si una gramática dada
está o no en formal normal de Chomsky.*)


let es_fnc (Gic(_, _, reglas, inicial)) = 
   let rec aux reglas = match reglas with
     [] -> true
    | p :: rest ->  match p with
                  | Regla_gic (No_terminal _ , [No_terminal _ ; No_terminal _] ) -> aux rest
                  | Regla_gic (No_terminal _ , [Terminal _]) -> aux rest
                  | Regla_gic (inicial, []) -> aux rest
                  | _ -> false
   in aux (list_of_conjunto reglas);;




(*Implemente una función cyk : Auto.simbolo list -> Auto.gic -> bool que, dada
una lista de símbolos de entrada y una gramática, indique si la cadena de entrada pertenece
o no al lenguaje generado por la gramática.
Lo primero que debe hacer esta función es comprobar que la cadena de entrada tiene al
menos un símbolo, y que la gramática está en forma normal de Chomsky. Si no es así, la
función activará una excepción.*)



exception NotValidChain;;
exception NotInChomskyForm;;


let cyk cadena (Gic(_, _, reglas, inicial) as gic) = 
  let n = List.length cadena in
  if n = 0 then raise NotValidChain
  else if (not (es_fnc gic)) then raise NotInChomskyForm
  else
    let boxes = Array.make_matrix n n (conjunto_vacio) in
    let llenar column simbol = 
      List.iter (fun (Regla_gic(x,y)) -> if y = [simbol] then boxes.(column).(0) <- agregar x boxes.(column).(0) 
                ) (list_of_conjunto reglas) in
    List.iteri llenar cadena;
    for j = 2 to n do 
      for i = 1 to n - j + 1 do 
        for k = 1 to j - 1 do 
          let b_NoTerm = boxes.(i-1).(k-1) in
          let c_NoTerm = boxes.(i+k-1).(j-k-1) in
          let list_cartesianos = list_of_conjunto (cartesiano b_NoTerm c_NoTerm) in
          List.iter (fun (nT1,nT2) -> List.iter (fun (Regla_gic(x,y)) -> if y = [nT1; nT2] then 
                                                         boxes.(i-1).(j-1) <- agregar x boxes.(i-1).(j-1)
                    ) (list_of_conjunto reglas)) list_cartesianos
        done;
      done;
    done;
    pertenece inicial boxes.(0).(n-1);;



(*Revise su implementación anterior del algoritmo CYK, para definir ahora una nueva
función cyk plus : Auto.simbolo list -> Auto.gic -> bool * string list que,
o bien devuelve (false,[]), o bien devuelve true y la lista de todos los posibles árboles
de derivación de la cadena de entrada, expresados en forma de secuencias parentizadas de
símbolos*)



let recorrer_subArbol regla boxes inicial= 
   let rec aux regla boxes = match regla with
      | (Regla_gic(No_terminal x , [Terminal y] ) , _) -> "(" ^ x ^ " " ^ y ^ ")"
      | (Regla_gic(No_terminal x, [No_terminal y; No_terminal z]) , indices ) ->
                  
                  let i1 = List.nth indices 0 in
                  let j1 = List.nth indices 1 in
                  let i2 = List.nth indices 2 in 
                  let j2 = List.nth indices 3 in
                  let box1 = list_of_conjunto (snd boxes.(i1).(j1)) in
                  let box2 = list_of_conjunto (snd boxes.(i2).(j2)) in
                  let nueva_regla1 = List.find (fun (Regla_gic(w, _), _ ) -> No_terminal y = w) box1 in
                  let nueva_regla2 = List.find (fun (Regla_gic(v, _), _ ) -> No_terminal z = v) box2 in             
                  "(" ^ x ^ " " ^ aux nueva_regla1 boxes ^ " " ^ aux nueva_regla2 boxes ^ ")"
      | _ -> ""
   in aux regla boxes;;



let cyk_plus cadena (Gic(_, _, reglas, inicial) as gic) = 
  let n = List.length cadena in
  if n = 0 then raise NotValidChain
  else if (not (es_fnc gic)) then raise NotInChomskyForm
  else begin
    let boxes = Array.make_matrix n n (conjunto_vacio, conjunto_vacio) in
    let llenar column simbol = 
      List.iter (fun (Regla_gic(x,y)) -> if y = [simbol] then boxes.(column).(0) <- 
          let c1 = agregar x (fst boxes.(column).(0)) in
          let c2 = agregar (Regla_gic(x,y), []) (snd boxes.(column).(0))in
          ( c1 , c2)
       ) (list_of_conjunto reglas) in
    List.iteri llenar cadena;
    for j = 2 to n do 
      for i = 1 to n - j + 1 do 
        for k = 1 to j - 1 do 
          let b_NoTerm = fst boxes.(i-1).(k-1) in
          let c_NoTerm = fst boxes.(i+k-1).(j-k-1) in
          let list_cartesianos = list_of_conjunto (cartesiano b_NoTerm c_NoTerm) in
           List.iter (fun (nT1,nT2) -> List.iter (fun (Regla_gic(x,y)) -> if y = [nT1; nT2] then boxes.(i-1).(j-1) <-
                  let d1 = agregar x (fst boxes.(i-1).(j-1)) in
                  let d2 = agregar (Regla_gic(x,y), [i-1;k-1;i+k-1;j-k-1]) (snd boxes.(i-1).(j-1)) in
                  (d1, d2)
                                       ) (list_of_conjunto reglas)
                    
           ) list_cartesianos
        done;
      done;
    done;
    if not(pertenece inicial (fst boxes.(0).(n-1))) then
      (false, [])
    else
      let raices = List.filter (fun ((Regla_gic(x,y)), _ ) -> x = inicial) (list_of_conjunto(snd boxes.(0).(n-1))) in
      let list_subarboles = List.map (fun x -> recorrer_subArbol x boxes inicial) raices in
      (true, list_subarboles)     
  end;;


(*Revise su implementación anterior del algoritmo CYK, para definir una nueva función
cyk prob : Auto.simbolo list -> gicp -> bool * (string * float) list que, o
bien devuelve (false,[]), o bien devuelve true y la lista de todos los posibles árboles
de derivación de la cadena de entrada, expresados en forma de secuencias parentizadas de
símbolos y acompañados de su correspondiente probabilidad de análisis.*)



type regla_gicp =
   Regla_gicp of (simbolo * simbolo list * float);;

type gicp =
   Gicp of (simbolo conjunto * simbolo conjunto * regla_gicp conjunto * simbolo);;
   

let gicp_of_gic (Gic(noTerms, terms, reglas , inicial)) probs = 
    let rec aux list_reglas_prob acc = match list_reglas_prob with 
    [], [] -> Gicp(noTerms, terms, acc, inicial)
    | Regla_gic(x,y) :: r1, z :: r2 -> let nuevas_reglas = agregar (Regla_gicp(x,y,z)) acc in 
                              aux (r1,r2) nuevas_reglas
    | _ , _ -> Gicp(noTerms, terms, acc, inicial)
    in aux ((list_of_conjunto reglas),probs) conjunto_vacio;;


let gic_of_gicp (Gicp(noTerms, terms, reglas, inicial)) =
  let rec aux reglas acc = match reglas with
  [] -> Gic(noTerms, terms, acc, inicial)
  | Regla_gicp(x, y, _) :: rest -> let nuevas_reglas = agregar (Regla_gic(x,y)) acc in
                           aux rest nuevas_reglas
  in aux (list_of_conjunto reglas) conjunto_vacio;;

  
let rec recorrer_subArbol_prob regla boxes inicial = 
  let rec aux regla boxes = match regla with
    | (Regla_gicp(No_terminal x, [Terminal y], prob), _) -> ("(" ^ x ^ " " ^ y ^ ")", prob)
    | (Regla_gicp(No_terminal x, [No_terminal y; No_terminal z], prob), indices) ->
      let i1 = List.nth indices 0 in
      let j1 = List.nth indices 1 in
      let i2 = List.nth indices 2 in 
      let j2 = List.nth indices 3 in
      let box1 = list_of_conjunto (snd boxes.(i1).(j1)) in
      let box2 = list_of_conjunto (snd boxes.(i2).(j2)) in
      let nueva_regla1 = List.find (fun (Regla_gicp(w, _, _), _) -> No_terminal y = w) box1 in
      let nueva_regla2 = List.find (fun (Regla_gicp(v, _, _), _) -> No_terminal z = v) box2 in             
      let (subArbol1, prob1) = aux nueva_regla1 boxes in
      let (subArbol2, prob2) = aux nueva_regla2 boxes in
      ("(" ^ x ^ " " ^ subArbol1 ^ " " ^ subArbol2 ^ ")", prob *. prob1 *. prob2)
    | _ -> ("", 1.0)
  in aux regla boxes;;


let cyk_prob cadena (Gicp(_, _, reglas, inicial) as gicp) = 
  let n = List.length cadena in
  if n = 0 then raise NotValidChain
  else if (not (es_fnc (gic_of_gicp gicp))) then raise NotInChomskyForm
  else begin
    let boxes = Array.make_matrix n n (conjunto_vacio, conjunto_vacio) in
    let llenar column simbol = 
      List.iter (fun (Regla_gicp(x,y,z)) -> if y = [simbol] then boxes.(column).(0) <- 
          let c1 = agregar x (fst boxes.(column).(0)) in
          let c2 = agregar (Regla_gicp(x,y,z), []) (snd boxes.(column).(0))in
          ( c1 , c2)
       ) (list_of_conjunto reglas) in
    List.iteri llenar cadena;
    for j = 2 to n do 
      for i = 1 to n - j + 1 do 
        for k = 1 to j - 1 do 
          let b_NoTerm = fst boxes.(i-1).(k-1) in
          let c_NoTerm = fst boxes.(i+k-1).(j-k-1) in
          let list_cartesianos = list_of_conjunto (cartesiano b_NoTerm c_NoTerm) in
           List.iter (fun (nT1,nT2) -> List.iter (fun (Regla_gicp(x,y,z)) -> if y = [nT1; nT2] then boxes.(i-1).(j-1) <-
                  let d1 = agregar x (fst boxes.(i-1).(j-1)) in
                  let d2 = agregar (Regla_gicp(x,y,z), [i-1;k-1;i+k-1;j-k-1]) (snd boxes.(i-1).(j-1)) in
                  (d1, d2)
                                       ) (list_of_conjunto reglas)
           ) list_cartesianos
        done;
      done;
    done;
    if not(pertenece inicial (fst boxes.(0).(n-1))) then
      (false, [])
    else
      let raices = List.filter (fun ((Regla_gicp(x,_,_)), _ ) -> x = inicial) (list_of_conjunto(snd boxes.(0).(n-1))) in
      let list_subarboles = List.map (fun x -> 
                                      let (recorrido, prob) = recorrer_subArbol_prob x boxes inicial in
                                      let prob_format = Printf.sprintf "%.5f" prob in
                                      let prob_format_float = float_of_string prob_format in
                                      (recorrido, prob_format_float)
                            ) raices in
      (true, list_subarboles)     
  end;;


