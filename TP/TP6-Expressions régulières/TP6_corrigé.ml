#use (* À MODIFIER/ *)"regex.ml";;

let e0 = parse "(a|ba*)*|ab*a";;

let e1 = parse "a(a|b)*b";;

(* Si l'expression est atomique, elle n'est pas modifiée car elle vérifie
   les conditions d'une expression normale. Sinon, on remarque que L(ef) = ∅
   si et seulement si L(e) = ∅ OU L(f) = ∅. Dans le cas contraire, 
   l'expression normalisée de ef est la concaténation des expressions
   normalisées. De même, L(e|f) = ∅ si et seulement si L(e) = ∅ ET 
   L(f) = ∅, et si L(e) = ∅, alors L( e* ) = {ε}. *)
let rec normaliser = function
    | Concat (e, f) -> let en = normaliser e and fn = normaliser f in
                       if en = Vide || fn = Vide then Vide
                       else Concat (en, fn)
    | Union (e, f)  -> let en = normaliser e and fn = normaliser f in
                       if en = Vide then fn
                       else if fn = Vide then en
                       else Union (en, fn)
    | Etoile e      -> let en = normaliser e in
                       if en = Vide then Epsilon
                       else Etoile en
    | e                                   -> e;;

(* Le test de vacuité est simple à faire une fois qu'on connaît
   l'expression normalisée. *)
let est_vide e = normaliser e = Vide;;

let _ = assert (est_vide (parse "#"));
        assert (not (est_vide e0));; 

(* On parcourt l'expression régulière et on ne compte que les
   feuilles qui sont des lettres. Le reste de la fonction se
   fait comme le calcul de la taille d'un arbre binaire. *)
let rec nombre_lettres = function
    | Vide | Epsilon             -> 0
    | Lettre _                   -> 1
    | Union(e, f) | Concat(e, f) -> nombre_lettres e + nombre_lettres f
    | Etoile e                   -> nombre_lettres e;;

let _ = assert (nombre_lettres e0 = 6);;

(* On commence par calculer le nombre k de lettres et on crée un
   tableau de k lettres. Ensuite, on parcourt l'expression
   régulière, et chaque fois qu'on tombe sur une lettre, on
   modifie la prochaine case du tableau avec cette lettre, et
   on crée une nouvelle expression avec l'indice du tableau
   correspondant. *)
let lineariser e =
    let k = nombre_lettres e in
    let t = Array.make k 'a' in
    let i = ref 0 in
    let rec aux_lin = function
        | Vide          -> Vide
        | Epsilon       -> Epsilon
        | Lettre a      -> t.(!i) <- a; incr i; Lettre (!i - 1)
        | Union (e, f)  -> Union (aux_lin e, aux_lin f)
        | Concat (e, f) -> Concat (aux_lin e, aux_lin f)
        | Etoile e      -> Etoile (aux_lin e) in
    aux_lin e, t;;

(* Pour les fonctions calculant V, P, S et F, on utilise les 
   formules récursives vues en cours. *)
let rec mot_vide = function
    | Vide | Lettre _    -> false
    | Epsilon | Etoile _ -> true
    | Union (e, f)       -> mot_vide e || mot_vide f
    | Concat (e, f)      -> mot_vide e && mot_vide f;;

(* On se contente de créer un tableau avec des OU logiques. *)
let union t1 t2 =
    let n = Array.length t1 in
    assert (n = Array.length t2);
    let t = Array.make n false in
    for i = 0 to n - 1 do
        t.(i) <- t1.(i) || t2.(i)
    done;
    t;;

(* On fait attention pour la concaténation, car P(ef) = P(e) ∪ (V(e)P(f)). *)
let rec prefixes n = function
    | Vide | Epsilon -> Array.make n false
    | Lettre i       -> let t = Array.make n false in
                        t.(i) <- true; t
    | Union(e, f)    -> union (prefixes n e) (prefixes n f)
    | Concat(e, f)   -> let t1 = prefixes n e in
                        if mot_vide e then union t1 (prefixes n f)
                                      else t1
    | Etoile e       -> prefixes n e;;

(* Comme la fonction précédente. *)
let rec suffixes n = function
    | Vide | Epsilon -> Array.make n false
    | Lettre i       -> let t = Array.make n false in
                        t.(i) <- true; t
    | Union(e, f)    -> union (suffixes n e) (suffixes n f)
    | Concat(e, f)   -> let t2 = suffixes n f in
                        if mot_vide f then union (suffixes n e) t2
                                      else t2
    | Etoile e       -> suffixes n e;;

(* On crée une fonction qui fait l'« union » de deux matrices. *)
let union_matrice m1 m2 =
    let n = Array.length m1 in
    let m = Array.make_matrix n n false in
    for i = 0 to n - 1 do
        m.(i) <- union m1.(i) m2.(i)
    done;
    m;;

(* À nouveau, la concaténation doit bien être traitée, car F(ef) = F(e) ∪ F(f) ∪ S(e)P(f). 
   De même pour l'étoile, car F( e * ) = F(e) ∪ S(e)P(e). *)
let rec facteurs n = function
    | Vide | Epsilon | Lettre _ -> Array.make_matrix n n false
    | Union (e, f)              -> union_matrice (facteurs n e) (facteurs n f)
    | Concat (e, f)             -> let m = union_matrice (facteurs n e) (facteurs n f) in
                                   let suff = suffixes n e and pref = prefixes n f in
                                   for i = 0 to n - 1 do
                                       for j = 0 to n - 1 do
                                           m.(i).(j) <- m.(i).(j) || (suff.(i) && pref.(j))
                                       done
                                   done;
                                   m
    | Etoile e                  -> let m = facteurs n e in
                                   let suff = suffixes n e and pref = prefixes n e in
                                   for i = 0 to n - 1 do
                                       for j = 0 to n - 1 do
                                           m.(i).(j) <- m.(i).(j) || (suff.(i) && pref.(j))
                                       done
                                   done;
                                   m;;

type 'a afnd = { finaux : bool array;
                 delta : ('a * int) list array };;

(* On commence par calculer l'expression linéarisée et le tableau
   associé. On calcule les ensembles V, P, S, F pour la nouvelle
   expression, puis on crée le nouvel automate (il contient un état
   de plus que le nombre de lettres). L'état initial est final si 
   l'ensemble V est non vide. Pour créer les transitions, on 
   utilise le tableau t pour savoir par quelle lettre faire
   l'étiquetage. *)
let glushkov e =
    let elin, t = lineariser e in
    let n = Array.length t in
    let ve = mot_vide elin and
        pe = prefixes n elin and
        se = suffixes n elin and
        fe = facteurs n elin in
    let finaux = Array.make (n + 1) false and
        delta = Array.make (n + 1) [] in
    finaux.(0) <- ve;
    let transition q a q' = delta.(q) <- (a, q') :: delta.(q) in
    for q = 1 to n do
        if pe.(q - 1) then transition 0 t.(q - 1) q;
        finaux.(q) <- se.(q - 1);
        for q' = 1 to n do
            if fe.(q - 1).(q' - 1) then transition q t.(q' - 1) q'
        done
    done;
    {finaux = finaux; delta = delta};;

let g0 = glushkov e0;;

let g1 = glushkov e1;;

(* On parcourt les listes d'adjacence de chaque état dans X, et
   on ne garde que les voisins par une transition de lettre a. *)
let delta_ens aut x a =
    let n = Array.length x in
    let x' = Array.make n false in
    for q = 0 to n - 1 do
        let traiter (b, q') = x'.(q') <- x'.(q') || a = b in
        if x.(q) then List.iter traiter aut.delta.(q)
    done;
    x';;

(* On applique la fonction précédente pour chaque lettre de u. *)
let delta_etoile aut x u =
    let x' = ref x in
    for i = 0 to String.length u - 1 do
        x' := delta_ens aut !x' u.[i]
    done;
    !x';;

(* On commence par créer l'ensemble I sous forme d'un tableau de 
   booléens (qui ne contient qu'une seule valeur true car il n'y
   a qu'un seul état initial). Puis, on calcule Δ*(I, u) et on 
   parcourt le tableau pour déterminer s'il existe un état final
   dans Δ*(I, u). *)
let reconnu aut u =
    let n = Array.length aut.finaux in
    let x = Array.make n false in
    x.(0) <- true;
    let x' = delta_etoile aut x u in
    let i = ref 0 in
    while !i < n && (not x'.(!i) || not aut.finaux.(!i)) do
        incr i
    done;
    !i < n;;

let _ = assert (reconnu g0 "ababaaaaaba");
        assert (reconnu g1 "abbbabaaab");
        assert (not (reconnu g1 "bbaaaabaaaa"));;

type 'a etat = { lettre : 'a option;
                 mutable sortie1 : 'a etat option;
                 mutable sortie2 : 'a etat option };;

type 'a afndn = { initial : 'a etat;
                  final : 'a etat};;

(* On applique la construction vue en exercice.*)
let rec thompson = function
    | Vide 
       -> let initial = {lettre = None; sortie1 = None; sortie2 = None}
          and final = {lettre = None; sortie1 = None; sortie2 = None} in
          {initial; final}
    | Epsilon 
       -> let initial = {lettre = None; sortie1 = None; sortie2 = None}
          and final = {lettre = None; sortie1 = None; sortie2 = None} in
          initial.sortie1 <- Some final;
          {initial; final}
    | Lettre a
       -> let initial = {lettre = Some a; sortie1 = None; sortie2 = None}
          and final = {lettre = None; sortie1 = None; sortie2 = None} in
          initial.sortie1 <- Some final;
          {initial; final}
    | Concat(e1, e2) 
       -> let t1 = thompson e1 and t2 = thompson e2 in
          t1.final.sortie1 <- Some t2.initial;
          {initial = t1.initial; final = t2.final}
    | Union(e1, e2) 
       -> let t1 = thompson e1 and t2 = thompson e2 in
          let initial = {lettre = None; 
                        sortie1 = Some t1.initial; 
                        sortie2 = Some t2.initial}
          and final = {lettre = None; sortie1 = None; sortie2 = None} in
          t1.final.sortie1 <- Some final;
          t2.final.sortie1 <- Some final;
          {initial; final}
    | Etoile e 
       -> let th = thompson e in
          let final = {lettre = None; sortie1 = None; sortie2 = None} in
          let initial = {lettre = None; 
                          sortie1 = Some th.initial; 
                          sortie2 = Some final} in
          th.final.sortie1 <- Some th.initial;
          th.final.sortie2 <- Some final;
          {initial; final};;

(* On procède ici par backtracking lorsqu'un état possède deux transitions
   sortantes, qui sont alors toutes deux étiquetées par ε. *)
let reconnu_thompson th u =
    let n = String.length u in
    let rec accepte q i = match q.lettre, q.sortie1, q.sortie2 with
        | None, None, None       -> q == th.final && i = n
        | Some a, Some p, None   -> i < n && u.[i] = a && accepte p (i + 1)
        | None, Some p, None     -> accepte p i
        | None, Some p1, Some p2 -> accepte p1 i || accepte p2 i
        | _ -> failwith "L'automate n'est pas normalisé" in
    accepte th.initial 0;;

let th0 = thompson e0;;

let th1 = thompson e1;;

let _ = assert (reconnu_thompson th0 "ababaaaaaba");
        assert (reconnu_thompson th1 "abbbabaaab");
        assert (not (reconnu_thompson th1 "bbaaaabaaaa"));;

(* La fonction est correcte avec les automates de Thompson des exemples
   précédents, mais peut avoir une boucle infinie, par exemple si on 
   construit l'automate de ( a* )*. *)

(* Pour éviter le problème précédent, on modifie l'expression régulière *)
let rec reecriture = function
    | Vide          -> Vide
    | Lettre a      -> Lettre a
    | Epsilon       -> Union (Vide, Epsilon)
    | Concat (e, f) -> begin match reecriture e, reecriture f with                          
        | Union (e', Epsilon), Union (f', Epsilon) -> 
              Union (Union (Union (Concat (e', f'), e'), f'), Epsilon)
        | Union (e', Epsilon), f'                  ->
              Union (Concat (e', f'), f')
        | e', Union (f', Epsilon)                  ->
              Union (Concat (e', f'), e')
        | e', f'                                   ->
              Concat (e', f') end
    | Union (e, f)  -> begin match reecriture e, reecriture f with
        | Union (e', Epsilon), Union (f', Epsilon)               
        | Union (e', Epsilon), f'          
        | e', Union (f', Epsilon)                  ->
              Union (Union (e', f'), Epsilon)
        | e', f'                                   ->
              Union (e', f') end
    | Etoile e      -> match reecriture e with
        | Union (e', Epsilon) | e' ->
              Union (Concat (e', Etoile e'), Epsilon);;

let thompson_bis e = thompson (reecriture e);;

let e2 = parse "a**";;

let th2 = thompson_bis e2;;

let _ = assert (reconnu_thompson th2 "aaa");;


















