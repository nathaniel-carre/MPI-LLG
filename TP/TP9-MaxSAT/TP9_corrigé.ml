type litteral = Var of int | Neg of int;;

type clause = litteral list;;

type fnc = clause list;;

let phi = [[Neg 0; Var 1; Neg 2];
           [Var 0; Neg 1; Var 2];
           [Var 0; Var 1; Var 2];
           [Neg 0; Neg 1]];;

let lire_fichier fichier =
    (* On lit chaque ligne et on l'ajoute à une liste. *)
    let canal = open_in fichier in
    let lst = ref [] in
    try while true do
            lst := input_line canal :: !lst
        done;
        []
    with End_of_file -> close_in canal; List.rev !lst;;

let fichier = "TP9_formules.txt";;

let fnc_ligne ligne =
    (* Pour déterminer une clause, on coupe selon la virgule, et
       on reconstruit chaque littéral. On distingue selon qu'il y ait
       un signe - ou non. Pour construire une FNC, on découpe selon
       le point-virgule et on applique le procédé précédent sur chaque
       clause. *)
    let ligne = List.hd (String.split_on_char '\r' ligne) in
    let lst_clause = String.split_on_char ';' ligne in
    let clause s =
        let lst_litt = String.split_on_char ',' s in
        let convertir litt =
            if litt.[0] = '-' then 
               let n = String.length litt in
               let var = String.sub litt 1 (n - 1) in
               Neg (int_of_string var)
            else Var (int_of_string litt) in
        List.map convertir lst_litt in
    List.map clause lst_clause;;

let tab_fnc fichier =
    let lecture = lire_fichier fichier in
    let lst = List.map fnc_ligne lecture in
    Array.of_list lst;;

let taille_V phi =
    (* On détermine la variable d'indice maximal et on ajoute 1. *)
    let rec maximum maxi = function
        | []        -> maxi
        | [] :: psi -> maximum maxi psi
        | (Var i :: q) :: psi | (Neg i :: q) :: psi ->
            maximum (max maxi i) (q :: psi) in
    maximum 0 phi + 1;;

let evaluer c mu =
    (* On note que le ou logique correspond à un max sur les images
       par mu. *)
    let eval_litt = function
        | Var i -> mu.(i)
        | Neg i -> 1 - mu.(i) in
    List.fold_left (fun a b -> max a (eval_litt b)) 0 c;;

let taille_J phi mu =
    (* On compte le nombre de clauses satisfaites. *)
    List.fold_left (fun a c -> a + evaluer c mu) 0 phi;;

let valuation n k =
    let mu = Array.make n 0 in
    let h = ref k in
    for i = 0 to n - 1 do
        mu.(i) <- !h mod 2;
        h := !h / 2
    done;
    mu;;

let maxsat_naif phi =
    (* On énumère toutes les valuations et on calcule la taille
       de J. On garde la maximale. *)
    let taille_max = ref 0 and
        mu_max = ref [||] in
    let n = taille_V phi in
    for k = 0 to 1 lsl n - 1 do
        let mu = valuation n k in
        let tj = taille_J phi mu in
        if tj > !taille_max then begin
           taille_max := tj;
           mu_max := mu
        end
    done;
    !mu_max;;

let maxsat_alea phi =
    let n = taille_V phi in
    Array.init n (fun _ -> Random.int 2);;

let simulation phi m =
    let opt_tj = taille_J phi (maxsat_naif phi) in
    let somme_tj = ref 0 in
    for _ = 0 to m - 1 do
        let mu = maxsat_alea phi in
        somme_tj := !somme_tj + taille_J phi mu
    done;
    float_of_int !somme_tj /. float_of_int (m * opt_tj);;

let proba_condi c mu i =
    (* On distingue selon les cas qui sont décrits. *)
    let rec repartition = function
        | []         -> 0, 0
        | Var j :: q -> let a, b = repartition q in
                        if j <= i then
                           if mu.(j) = 1 then 1, b
                                         else a, b
                        else
                           a, b + 1
        | Neg j :: q -> let a, b = repartition q in
                        if j <= i then
                           if mu.(j) = 0 then 1, b
                                         else a, b
                        else
                           a, b + 1 in
    let a, b = repartition c in
    if a = 1 then 1.
    else if b = 0 then 0.
    else 1. -. 1. /. (2. ** (float_of_int b));;

let esperance_condi phi mu i =
    List.fold_left (fun a c -> a +. proba_condi c mu i) 0. phi;;

let maxsat_2approx phi =
    let n = taille_V phi in
    let mu = Array.make n 0 in
    for i = 0 to n - 1 do
        let e1 = esperance_condi phi mu i in
        mu.(i) <- 1;
        let e2 = esperance_condi phi mu i in
        if e2 < e1 then mu.(i) <- 0
    done;
    mu;;

let rec eval_partiel mu i = function
    (* Cette fonction prend en argument une valuation, un indice de variable
       et une clause et renvoie un quadruplet de booléens a, b, c, d tels que :
        - a détermine si xi apparaît ou non
        - b détermine si ¬xi apparaît ou non
        - c détermine si un littéral d'indice < i est déjà satisfaite dans la clause
        - d détermine s'il existe un littéral d'indice > i dans la clause. *)
    | []         -> (false, false, false, false)
    | Var j :: q -> let a, b, c, d = eval_partiel mu i q in
                    (j = i || a, b, j < i && mu.(i) = 1 || c, j > i || d)
    | Neg j :: q -> let a, b, c, d = eval_partiel mu i q in
                    (a, j = i || b, j < i && mu.(i) = 0 || c, j > i || d);;

let heuristique phi mu i =
    (* On compte le nombre de clauses qui ne peuvent pas être satisfaites, ainsi
       que le nombre de clauses qui sont réduites à un littéral xi ou ¬xi et dont
       aucune autre littéral ne peut satisfaire la clause. L'heuristique est alors
       le nombre total de clauses, moins le nombre de clauses non satisfiables, 
       moins le plus petit entre le nombre de clauses qui peuvent être satisfaites
       par xi ou par ¬xi. *)
    let non_satisfaite = ref 0 and
        contient_que_xi = ref 0 and
        contient_que_negxi = ref 0 in
    let incrementer (a, b, c, d) =
        if not a && not b && not c && not d then incr non_satisfaite;
        if a && not b && not c && not d then incr contient_que_xi;
        if not a && b && not c && not d then incr contient_que_negxi in
    List.iter (fun c -> incrementer (eval_partiel mu i c)) phi;
    List.length phi - !non_satisfaite - (min !contient_que_xi !contient_que_negxi);;

let branchement phi mu i =
    (* L'idée est similaire à la fonction précédente et on applique l'énoncé. *)
    let contient_xi = ref 0 and
        contient_negxi = ref 0 in
    let incrementer (a, b, c, d) =
        if a && not c then incr contient_xi;
        if b && not c then incr contient_negxi in
    List.iter (fun c -> incrementer (eval_partiel mu i c)) phi;
    if !contient_xi > !contient_negxi then 1 else 0;;

let maxsat_bnb phi =
    (* On écrit alors l'algorithme de Branch & Bound avec les deux heuristiques. *)
    let taille_max = ref 0 and
        mu_max = ref [||] in
    let n = taille_V phi in
    let mu = Array.make n 0 in
    let rec bnb i =
        if i = n then begin
           let h = taille_J phi mu in
           if h > !taille_max then begin
              taille_max := h;
              mu_max := Array.copy mu
           end
        end else begin
           let h = heuristique phi mu i in
           if h > !taille_max then begin
              let b = branchement phi mu i in
              mu.(i) <- b;
              bnb (i + 1);
              mu.(i) <- 1 - b;
              bnb (i + 1)
           end
        end in
    bnb 0;
    !mu_max;;