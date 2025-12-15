type litteral = Var of int | Neg of int

type clause = litteral list

type fnc = clause list

let phi = [[Neg 0; Var 1; Neg 2];
           [Var 0; Neg 1; Var 2];
           [Var 0; Var 1; Var 2];
           [Neg 0; Neg 1]]

let tab_ui u0 n = 
    (* On crée un tableau de taille n contenant les ui. *)
    let tab = Array.make n u0 in
    for i = 1 to n - 1 do
        tab.(i) <- (19_999_999 * tab.(i - 1)) mod 19_999_981
    done;
    tab

let somme_ui u0 n = 
    let s = ref (u0 mod 1000) and
        u = ref u0 in
    for i = 1 to n do
        u := (19_999_999 * !u) mod 19_999_981;
        s := (!s + !u) mod 1000
    done;
    !s


let tab = tab_ui 42 1_000_000

let clause i n k =
    (* On parcourt chaque indice entre 0 et n - 1 et pour
       chacun, on détermine si on garde xj, ¬xj ou non. *)
    let rec aux j =
        if j = n then []
        else
            let r = tab.(i + j) mod (2 * n) in
            if r >= k then aux (j + 1)
            else if r mod 2 = 0 then Var j :: aux (j + 1)
            else Neg j :: aux (j + 1)
    in
    aux 0

let formule i n m k =
    (* On se contente de suivre la définition. *)
    List.init m (fun j -> clause (i + j * n) n k)

let evaluer c mu =
    (* On note que le ou logique correspond à un max sur les images
       par mu. *)
    let eval_litt = function
        | Var i -> mu.(i)
        | Neg i -> 1 - mu.(i) in
    List.fold_left (fun a b -> max a (eval_litt b)) 0 c

let taille_J phi mu =
    (* On compte le nombre de clauses satisfaites. *)
    List.fold_left (fun a c -> a + evaluer c mu) 0 phi

let maxsat_backtracking phi n =
    (* On crée une valuation courante et deux références pour garder
       l'optimal en mémoire. *)
    let mu = Array.make n (-1) in
    let mu_max = ref [||] and
        tJmax = ref (-1) in
    (* La fonction auxiliaire compare la solution courante à la meilleure
       actuelle si elle est totale, et sinon essaie de la compléter avec un 0
       puis un 1. *)
    let rec backtrack i =
        if i = n then begin
            let tJ = taille_J phi mu in
            if tJ > !tJmax then (mu_max := Array.copy mu; tJmax := tJ)
        end else begin
            mu.(i) <- 0;
            backtrack (i + 1);
            mu.(i) <- 1;
            backtrack (i + 1);
        end
    in
    backtrack 0;
    !mu_max

let testex2 n m k =
    let phi = formule 0 n m k in
    let mu = maxsat_backtracking phi n in
    Printf.printf "%d\n" (taille_J phi mu)

let maxsat_alea i n = 
    Array.init n (fun j -> tab.(i + j) mod 2)

let simulation phi n m =
    (* On calcule une seule fois la solution optimale, puis 
       on calcule la taille des m solutions aléatoires. *)
    let opt_tj = taille_J phi (maxsat_backtracking phi n) in
    let somme_tj = ref 0 in
    for i = 0 to m - 1 do
        let mu = maxsat_alea (n * i) n in
        somme_tj := !somme_tj + taille_J phi mu
    done;
    float_of_int !somme_tj /. float_of_int (m * opt_tj)

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
    else 1. -. 1. /. (2. ** (float_of_int b))

let esperance_condi phi mu i =
    List.fold_left (fun a c -> a +. proba_condi c mu i) 0. phi

let maxsat_2approx phi n =
    let mu = Array.make n 0 in
    for i = 0 to n - 1 do
        let e1 = esperance_condi phi mu i in
        mu.(i) <- 1;
        let e2 = esperance_condi phi mu i in
        if e2 < e1 then mu.(i) <- 0
    done;
    mu

let rec eval_partiel mu i = function
    (* Cette fonction prend en argument une valuation, un indice de variable
       et une clause et renvoie un quadruplet de booléens a, b, c, d tels que :
        - a détermine si xi apparaît ou non
        - b détermine si ¬xi apparaît ou non
        - c détermine si un littéral d'indice < i est déjà satisfaite dans la clause
        - d détermine s'il existe un littéral d'indice > i dans la clause. *)
    | []         -> (false, false, false, false)
    | Var j :: q -> let a, b, c, d = eval_partiel mu i q in
                    (j = i || a, b, j < i && mu.(j) = 1 || c, j > i || d)
    | Neg j :: q -> let a, b, c, d = eval_partiel mu i q in
                    (a, j = i || b, j < i && mu.(j) = 0 || c, j > i || d)

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
    List.length phi - !non_satisfaite - (min !contient_que_xi !contient_que_negxi)

let branchement phi mu i =
    (* L'idée est similaire à la fonction précédente et on applique l'énoncé. *)
    let contient_xi = ref 0 and
        contient_negxi = ref 0 in
    let incrementer (a, b, c, d) =
        if a && not c then incr contient_xi;
        if b && not c then incr contient_negxi in
    List.iter (fun c -> incrementer (eval_partiel mu i c)) phi;
    if !contient_xi > !contient_negxi then 1 else 0

let maxsat_bnb phi n =
    (* On écrit alors l'algorithme de Branch & Bound avec les deux heuristiques. *)
    let taille_max = ref 0 and
        mu_max = ref [||] in
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
    !mu_max