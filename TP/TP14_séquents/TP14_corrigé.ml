type formule =
     | Top
     | Bot
     | Var of int
     | Neg of formule
     | Impl of formule * formule
     | Et of formule * formule
     | Ou of formule * formule

type valuation = int array

type sequent = 
     {gamma : formule list; 
      gamma_var : bool array;
      delta : formule list;
      delta_var : bool array}

(* On se contente d'appliquer la formule récursive de l'évaluation d'une formule
   propositionnelle. *)
let rec interpretation mu = function
    | Top -> 1
    | Bot -> 0
    | Var i -> mu.(i)
    | Neg a -> 1 - interpretation mu a
    | Impl (a, b) -> max (1 - interpretation mu a) (interpretation mu b)
    | Et (a, b) -> min (interpretation mu a) (interpretation mu b)
    | Ou (a, b) -> max (interpretation mu a) (interpretation mu b)

(* On cherche le plus grand numéro de variable, et on renvoie sa valeur + 1, en 
   supposant que les variables sont indicées à partir de 0. *)
let rec nombre_variables = function
    | Top | Bot -> 0
    | Var i -> i + 1
    | Neg a -> nombre_variables a
    | Impl (a, b) | Et (a, b) | Ou (a, b)
        -> max (nombre_variables a) (nombre_variables b)

(* Cette fonction auxiliaire prend en argument une valuation et renvoie la 
   valuation suivante selon l'ordre lexicograaque. Cela correspond à une
   opération d'incrémentation si on considère un entier en représentation
   binaire (ici, bit de poids faible à gauche). On renvoie un booléen qui
   permet d'indiquer si c'était la dernière valuation (celle qui ne contient
   que des 1). *)
let incremente mu =
    let n = Array.length mu in
    let i = ref 0 in
    while !i < n && mu.(!i) = 1 do
        mu.(!i) <- 0;
        incr i
    done;
    if !i < n then (mu.(!i) <- 1; true)
              else false

(* Avec la fonction précédente, on procède de manière naïve : on part d'une
   valuation ne contenant que des 0, et on l'incrémente jusqu'à trouver une
   valuation qui ne satisfait pas la formule. Si c'est le cas, la formule
   n'est pas une tautologie. *)
let tautologie a =
    let n = nombre_variables a in
    let mu = Array.make n 0 in
    let b = ref (interpretation mu a = 1) in
    while !b && incremente mu do
        b := interpretation mu a = 1
    done;
    !b

(* Dans un premier temps, on calcule le nombre de variable. Il s'agit de calculer
   le maximum des nombres de variables de toutes les formules (ici, à l'aide d'un
   fold_left). Ensuite, on peut créer des tableaux de false et créer le séquent. *)
let creer_sequent gamma delta =
    let n = List.fold_left (fun a b -> max a (nombre_variables b)) 0 (gamma @ delta) in
    let t = Array.make n false in
    {gamma = gamma; gamma_var = t; delta = delta; delta_var = t}

let seq1 = creer_sequent [] [Ou(Var 0, Neg(Var 0))]
let seq2 = creer_sequent [Et(Var 0, Neg(Var 0))] []
let seq3 = creer_sequent [Ou(Neg(Var 0), Neg(Var 1))] [Neg(Et(Var 0, Var 1))]
let seq4 = creer_sequent [Neg(Et(Var 0, Var 1))] [Ou(Neg(Var 0), Neg(Var 1))]
let seq5 = creer_sequent [] [Impl(Impl(Impl(Var 0, Var 1), Var 0), Var 0)]

let seqs = [|seq1; seq2; seq3; seq4; seq5|]

(* On suit l'indication. La formule a correspond à la conjonction des formules de Γ.
   On fait attention à bien prendre en compte les variables qui sont indiquée à true
   dans le tableau gamma_var. On procède de même pour Δ, puis on vérifie si a → b
   est une tautologie. *)
let valide seq =
    let a = ref (List.fold_left (fun a b -> Et (a, b)) Top seq.gamma) in
    let b = ref (List.fold_left (fun a b -> Ou (a, b)) Bot seq.delta) in
    for i = 0 to Array.length seq.gamma_var - 1 do
        if seq.gamma_var.(i) then a := Et (!a, Var i);
        if seq.delta_var.(i) then b := Ou (!b, Var i)
    done;
    tautologie (Impl (!a, !b))

let _ = for i = 0 to 4 do assert(valide seqs.(i)) done

(* Pas de difficulté particulière pour ces fonctions : c'est un peu répétitif,
   mais il suffit de recréer un ou deux séquents selon la règle correspondante.
   Les tableaux ne sont pas modifiés. *)
let impl_gauche seq = match seq.gamma with
    | Impl (a, b) :: q -> 
        {gamma = q; gamma_var = seq.gamma_var;
         delta = a :: seq.delta; delta_var = seq.delta_var},
        {gamma = b :: q; gamma_var = seq.gamma_var;
         delta = seq.delta; delta_var = seq.delta_var}
    | _ -> failwith "Erreur règle"

let impl_droite seq = match seq.delta with
    | Impl (a, b) :: q -> 
        {gamma = a :: seq.gamma; gamma_var = seq.gamma_var;
         delta = b :: q; delta_var = seq.delta_var}
    | _ -> failwith "Erreur règle"

let et_gauche seq = match seq.gamma with
    | Et (a, b) :: q -> 
        {gamma = a :: b :: q; gamma_var = seq.gamma_var;
         delta = seq.delta; delta_var = seq.delta_var}
    | _ -> failwith "Erreur règle"

let et_droite seq = match seq.delta with
    | Et (a, b) :: q -> 
        {gamma = seq.gamma; gamma_var = seq.gamma_var;
         delta = a :: q; delta_var = seq.delta_var},
        {gamma = seq.gamma; gamma_var = seq.gamma_var;
         delta = b :: q; delta_var = seq.delta_var}
    | _ -> failwith "Erreur règle"

let ou_gauche seq = match seq.gamma with
    | Ou (a, b) :: q -> 
        {gamma = a :: q; gamma_var = seq.gamma_var;
         delta = seq.delta; delta_var = seq.delta_var},
        {gamma = b :: q; gamma_var = seq.gamma_var;
         delta = seq.delta; delta_var = seq.delta_var}
    | _ -> failwith "Erreur règle"

let ou_droite seq = match seq.delta with
    | Ou (a, b) :: q -> 
        {gamma = seq.gamma; gamma_var = seq.gamma_var;
         delta = a :: b :: q; delta_var = seq.delta_var}
    | _ -> failwith "Erreur règle"

let neg_gauche seq = match seq.gamma with
    | Neg a :: q -> 
        {gamma = q; gamma_var = seq.gamma_var;
         delta = a :: seq.delta; delta_var = seq.delta_var}
    | _ -> failwith "Erreur règle"

let neg_droite seq = match seq.delta with
    | Neg a :: q -> 
        {gamma = a :: seq.gamma; gamma_var = seq.gamma_var;
         delta = q; delta_var = seq.delta_var}
    | _ -> failwith "Erreur règle"

let aff_gauche seq = match seq.gamma with
    | a :: q -> 
        {gamma = q; gamma_var = seq.gamma_var;
         delta = seq.delta; delta_var = seq.delta_var}
    | _ -> failwith "Erreur règle"

let aff_droite seq = match seq.delta with
    | a :: q -> 
        {gamma = seq.gamma; gamma_var = seq.gamma_var;
         delta = q; delta_var = seq.delta_var}
    | _ -> failwith "Erreur règle"

(* On applique l'algorithme qui est décrit informellement. On relance un
   ou deux appels récursifs selon la règle à utiliser. *)
let rec prouvable seq = match seq.gamma, seq.delta with
    | [], []      -> 
        let b = ref false and i = ref 0 in
        while !i < Array.length seq.gamma_var && not !b do
            b := seq.gamma_var.(!i) && seq.delta_var.(!i);
            incr i
        done;
        !b
    | a :: q, _ -> begin match a with
        | Bot -> true
        | Top -> prouvable (aff_gauche seq)
        | Var i when seq.delta_var.(i) -> true
        | Var i -> 
            let gv = Array.copy seq.gamma_var in
            gv.(i) <- true;
            prouvable {gamma = q; gamma_var = gv; 
                       delta = seq.delta; delta_var = seq.delta_var}
        | Impl (a, b) -> 
            let s1, s2 = impl_gauche seq in
            prouvable s1 && prouvable s2
        | Et (a, b) -> prouvable (et_gauche seq)
        | Ou (a, b) -> 
            let s1, s2 = ou_gauche seq in
            prouvable s1 && prouvable s2
        | Neg a -> prouvable (neg_gauche seq) end
    | _, a :: q -> begin match a with
        | Bot -> prouvable (aff_droite seq)
        | Top -> true
        | Var i when seq.gamma_var.(i) -> true
        | Var i -> 
            let dv = Array.copy seq.gamma_var in
            dv.(i) <- true;
            prouvable {gamma = seq.gamma; gamma_var = seq.gamma_var;
                       delta = q; delta_var = dv}
        | Impl (a, b) -> prouvable (impl_droite seq)
        | Et (a, b) -> 
            let s1, s2 = et_droite seq in
            prouvable s1 && prouvable s2
        | Ou (a, b) -> prouvable (ou_droite seq)
        | Neg a -> prouvable (neg_droite seq) end

let _ = for i = 0 to 4 do assert(prouvable seqs.(i)) done
