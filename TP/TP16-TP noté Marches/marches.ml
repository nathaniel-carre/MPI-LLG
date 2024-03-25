(* Question 1 *)

(* Une fonction qui renvoie un tableau contenant les n premières
   valeurs de la suite (un). *)
let tab_u u0 n =
    let tab = Array.make n u0 in
    for i = 1 to n - 1 do
        tab.(i) <- (909_091 * tab.(i - 1)) mod 1_010_101_039
    done;
    tab

let q1 u0 =
    let tab = tab_u u0 7_654_322 in
    let traiter n =
        Printf.printf "u_%d = %d\n" n (tab.(n) mod 1000) in
    List.iter traiter [1; 12; 1_234; 7_654_321]

(* Question 2 *)

(* On écrit une fonction auxiliaire qui garde en mémoire la valeur
   du dernier élément rajouté. On peut utiliser -1 comme valeur de
   base, car les valeurs manipulées sont positives. *)
let incr_list lst =
    let rec incr_aux der = function
        | [] -> []
        | x :: q when x <= der -> incr_aux der q
        | x :: q -> x :: incr_aux x q in
    incr_aux (-1) lst

let somme_liste lst =
    List.fold_left (+) 0 lst

let q2 u0 =    
    let traiter n =
        let tab = Array.map (fun x -> x mod 9) (tab_u u0 n) in
        let s = somme_liste (incr_list (Array.to_list tab)) in
        Printf.printf "Pour n = %d, la somme vaut %d\n" n s in
    List.iter traiter [3; 6; 123; 1234]

(* Question 3 *)

(* Les pondérations sont sur les sommets et pas les arêtes. On représente
   un tel graphe comme un couple d'un tableau de listes d'adjacence et d'un
   tableau de poids, tous deux de même taille n. *)
let graphe n m p u0 =
    let tab = tab_u u0 (n * (m + 1)) in
    let g = Array.make n [] and
        poids = Array.init n (fun s -> tab.(n * m + s) mod p) in
    for s = 0 to n - 1 do
        let pot = List.init m (fun i -> tab.(s * m + i) mod n) in
        g.(s) <- incr_list pot
    done;
    g, poids

(* Renvoie la somme des degrés *)
let somme_deg g =
    Array.fold_left (+) 0 (Array.map List.length g)

let q3 u0 =
    let traiter (n, m, p) =
        let g, _ = graphe n m p u0 in
        let som = somme_deg g in
        Printf.printf "Pour n, m, p = %d, %d, %d, la somme des degrés vaut %d\n" n m p som in
    List.iter traiter [(6, 3, 10); (123, 7, 10); (1234, 10, 10); (10_001, 22, 10)]

(* Question 4 *)

(* On calcule le haché de la manière dont il est décrit *)
let hache (g, poids) =
    let hache_som s =
        let hache_vois t =
            (s + 1) * (t + poids.(t) * poids.(t)) in
        somme_liste (List.map hache_vois g.(s)) in
    let sommets = List.init (Array.length g) Fun.id in
    somme_liste (List.map hache_som sommets) mod 1000

let q4 u0 =
    let traiter (n, m, p) =
        let g, poids = graphe n m p u0 in
        let h = hache (g, poids) in
        Printf.printf "Pour n, m, p = %d, %d, %d, le haché vaut %d\n" n m p h in
    List.iter traiter [(6, 3, 10); (123, 7, 10); (1234, 10, 10); (10_001, 22, 10)]
 
(* Question 5 *)

(* Calcule une marche de taille l en partant depuis s, selon la stratégie pi *)
let rec marche g l pi s = match l with
    | 0 -> [s]
    | _ -> s :: marche g (l - 1) pi (pi s)

let q5 u0 =
    let traiter (n, m, p, l) =
        let g, poids = graphe n m p u0 in
        let pi s = List.hd g.(s) in
        let som = somme_liste (marche g l pi (n - 1)) mod 1000 in
        Printf.printf "Pour n, m, p, l = %d, %d, %d, %d, la marche est de somme %d\n" n m p l som in
    List.iter traiter [(6, 3, 10, 4); (123, 7, 10, 20); (1234, 10, 10, 100); (10_001, 22, 10, 5000)]

(* Question 6 *)

(* Le foncteur Set permet de créer des modules. On l'utilise pour créer des ensembles
   d'entiers. Attention, la syntaxe n'est pas aussi simple pour d'autres types de données. 
   On aurait pu utiliser des tables de hachage pour modéliser les ensembles, mais la manipulation
   avec Set est simplifiée. *)
module Ent = Set.Make(Int)

(* Calcule un pas de la marche. Ici, la variable agents est un tableau tel que agents.(s) est
   l'ensemble des agents présents sur s, et vivants est l'ensemble des sommets vivants. Pour que
   les déplacements soient simultanés, on crée une table de hachage qui garde en mémoire, pour
   chaque sommet, l'ensemble des agents qu'il reçoit. Les sommets vivants perdent leurs agents.
   Une fois la table calculée, on la parcourt pour mettre à jour les agents sur les sommets
   receveurs, et ceux dont le nombre d'agents dépasse le poids deviennent vivants. On renvoie
   l'ensemble des nouveaux sommets vivants. *)
let pas (g, poids) agents vivants =
    let pi s = List.hd g.(s) in
    let n_ag = Hashtbl.create 1 in
    let ajout s x = match Hashtbl.find_opt n_ag s with
        | None -> Hashtbl.add n_ag s x
        | Some y -> Hashtbl.replace n_ag s (Ent.union x y) in
    let traiter s =
        ajout (pi s) agents.(s);
        agents.(s) <- Ent.empty in
    Ent.iter traiter vivants;
    let n_vi = ref Ent.empty in
    let maj s x =
        agents.(s) <- Ent.union agents.(s) x;
        if Ent.cardinal agents.(s) > poids.(s) then
            n_vi := Ent.add s !n_vi in
    Hashtbl.iter maj n_ag;
    !n_vi

(* Pour faire une marche, on crée un tableau d'agents, chaque sommet contenant l'agent de
   son numéro, puis on fait l pas en utilisant la fonction précédente. La fonction renvoie
   le couple des sommets vivants et des positions d'agents (nécessaire pour Q7). *)
let marche_agents (g, poids) l =
    let n = Array.length g in
    let agents = Array.init n (fun i -> Ent.add i Ent.empty) in
    let vivants = ref Ent.empty in
    for s = 0 to n - 1 do
        if poids.(s) = 0 then
            vivants := Ent.add s !vivants
    done;
    for _ = 0 to l - 1 do
        vivants := pas (g, poids) agents !vivants
    done;
    !vivants, agents

let q6 u0 =
    let traiter (n, m, p, l) =
        let g, poids = graphe n m p u0 in
        let vivants, _ = marche_agents (g, poids) l in
        Printf.printf "Pour n, m, p, l = %d, %d, %d, %d, la marche contient %d sommets vivants\n" 
            n m p l (Ent.cardinal vivants) in
    List.iter traiter [(6, 3, 3, 4); (1234, 10, 3, 100); 
                       (10_001, 22, 10, 50_000); (100_001, 40, 10, 100_000)]

(* Question 7 *)

let q7 u0 =
    let traiter (n, m, p, l) =
        let g, poids = graphe n m p u0 in
        let _, agents = marche_agents (g, poids) l in
        let som = ref 0 in
        for s = 0 to Array.length g - 1 do
            Ent.iter (fun a -> som := (!som + a * s) mod 1000) agents.(s)
        done;
        Printf.printf "Pour n, m, p, l = %d, %d, %d, %d, la somme après marche vaut %d\n" 
            n m p l !som in
    List.iter traiter [(6, 3, 3, 4); (1234, 10, 3, 100); 
                       (10_001, 22, 10, 50_000); (100_001, 40, 10, 100_000)]

(* Question 8 *)

(* Les tailles de marche avec lesquelles on demande de travailler sont bien trop grandes
   pour qu'il soit judicieux de générer les marches comme précédemment (pour des raisons de
   temps ET d'espace). On remarque qu'au dela d'une certaine taille de marche, il y a des
   cycles (car la stratégie est sans mémoire). La fonction suivante calcule la prépériode et
   la taille du cycle (on se contente de garder en mémoire l'indice de chaque sommet déjà vu). *)
let cycle g l pi s0 =
    let n = Array.length g in
    let vus = Array.make n (-1) in
    let s = ref s0 and i = ref 0 in
    while vus.(!s) = -1 && !i < l do
        vus.(!s) <- !i;
        incr i;
        s := pi !s
    done;
    if vus.(!s) = -1 then l, 0
    else vus.(!s), !i - vus.(!s)

(* Dès lors, pour calculer la valeur demandée, il suffit de le faire pour chaque sommet présent
   dans la prépériode et dans le cycle, en utilisant des divisions entières pour les sommets du
   cycle pour connaître le nombre de fois qu'on les parcourt. *)
let valeur (g, poids) alpha l =
    let pi = fun s -> List.hd g.(s) in
    let prep, per = cycle g l pi 0 in
    let s = ref 0 and v = ref 0 in
    for i = 0 to prep - 1 do
        v := (!v + poids.(!s) * min alpha 1) mod 999;
        s := pi !s
    done;
    for i = prep to prep + per - 1 do
        v := (!v + poids.(!s) * min alpha ((l - prep) / per)) mod 999;
        s := pi !s
    done;
    !v

let q8 u0 =
    let traiter (n, m, p, alpha, l) =
        let g, poids = graphe n m p u0 in
        let v = valeur (g, poids) alpha l in
        Printf.printf "Pour n, m, p, α, l = %d, %d, %d, %d, %d la valeur vaut %d\n" 
            n m p l alpha v in
    List.iter traiter [(6, 3, 10, 2, 6); (123, 7, 10, 20, 100); 
                       (1234, 10, 10, 1000, 100_000);
                       (10_001, 22, 10, 10_000, 10_000_000);
                       (100_001, 40, 10, 100, 1_000_000_000_000);
                       (200_002, 50, 10, 100, 1_000_000_000_000)]

(* Question 9 *)

(* Cette fonction calcule la valeur d'une marche. On commence par déterminer le nombre
   d'occurrence de chaque élément de la marche, puis on parcourt la table de hachage pour
   calculer le score. *)
let valeur_marche (g, poids) alpha rho =
    let occ = Hashtbl.create 1 in
    let ajout s = match Hashtbl.find_opt occ s with
        | None -> Hashtbl.add occ s 1
        | Some x -> Hashtbl.replace occ s (1 + x) in
    List.iter ajout rho;
    Hashtbl.fold (fun s x v -> v + poids.(s) * min alpha x) occ 0

(* On procède alors par retour sur trace : depuis un sommet, on essaie de compléter la
   marche avec chacun de ses voisins. On arrête quand la marche a la longueur attendue.
   Notons qu'ici, la marche est à l'envers (mais ça ne change rien pour le calcule de la
   valeur). *)
let valeur_max (g, poids) alpha l =
    let maxv = ref 0 in
    let rec backtrack rho l s = match l with
        | 0 -> maxv := max !maxv (valeur_marche (g, poids) alpha (s :: rho))
        | _ -> List.iter (backtrack (s :: rho) (l - 1)) g.(s) in
    backtrack [] l 0;
    !maxv
    
let q9 u0 =
    let traiter (n, m, p, alpha, l) =
        let g, poids = graphe n m p u0 in
        let maxv = valeur_max (g, poids) alpha l in
        Printf.printf "Pour n, m, p, α, l = %d, %d, %d, %d, %d, la valeur max vaut %d\n" 
            n m p alpha l maxv in
    List.iter traiter [(6, 3, 10, 1, 6); (6, 3, 10, 6, 6); 
                       (1234, 10, 10, 2, 9); (10_001, 22, 10, 2, 4)]    

(* Question 10 *)

(* Pour calculer cette valeur, on procède par programmation dynamique : on pose f(l, s)
   la valeur maximale d'une marche de longueur l, terminant par s, et -infini s'il n'en
   existe pas. On a alors f(0, s) = poids[0] si s = 0 et -infini sinon. De plus, pour l ≥ 0,
   f(l + 1, s) = max{f(l, t) + poids[s] | (t, s) est une arête}. On cherche alors la valeur
   maximale de f(l, s) sur tous les sommets s. *)

(* Comme on aura besoin de connaître les antécédents d'un sommet, on écrit une fonction qui
   transpose le graphe. *)
let transpose g = 
    let n = Array.length g in
    let gt = Array.make n [] in
    for s = 0 to n - 1 do
        List.iter (fun t -> gt.(t) <- s :: gt.(t)) g.(s)
    done;
    gt

(* On applique l'algorithme de programmation dynamique décrit plus haut. À noter, on aurait
   pu n'utiliser que deux tableaux de taille n au lieu d'une matrice de taille (l + 1) × n,
   en ne conservant que les valeurs pour k et k+1, et en inversant le rôle des tableaux à chaque
   itération. *)
let valeur_prog_dyn (g, poids) l =
    let n = Array.length g and gt = transpose g in
    let f = Array.make_matrix (l + 1) n (-1) in
    f.(0).(0) <- poids.(0);
    for k = 1 to l do
        for s = 0 to n - 1 do
            let v = ref (-1) in
            let traiter t =
                if f.(k - 1).(t) > -1 then
                    v := max !v (f.(k - 1).(t) + poids.(s)) in
            List.iter traiter gt.(s);
            f.(k).(s) <- !v
        done
    done;
    Array.fold_left max (-1) f.(l)

let q10 u0 =
    let traiter (n, m, p, l) =
        let g, poids = graphe n m p u0 in
        let maxv = valeur_prog_dyn (g, poids) l in
        Printf.printf "Pour n, m, p, l = %d, %d, %d, %d, la valeur max vaut %d\n" 
            n m p l maxv in
    List.iter traiter [(6, 3, 10, 6); (6, 3, 10, 200); 
                       (1234, 10, 10, 40); (10_001, 22, 10, 20)]  

(* Question 11 *)

(* On commence par calculer le graphe h. *)
let graphe_h n m p u0 =
    let g, poids = graphe n m p u0 in
    let n = Array.length g in
    let h = Array.copy g in
    for s = 0 to n - 1 do
        if List.mem s h.(s) then begin
            h.(s) <- List.filter (fun t -> t <> s) h.(s);
            let sp1 = (s + 1) mod n in
            if not (List.mem sp1 h.(s)) then
                h.(s) <- sp1 :: h.(s)
        end
    done;
    h, poids

(* Ensuite, on procède par programmation dynamique à nouveau pour calculer les poids
   des chemins, donnés par la somme des poids des sommets qui le composent. Pour ce faire, 
   si pmax(l, s, t) représente le poids maximal d'un chemin de longueur l de s à t 
   (élémentaire ou non), alors : f(l, s, t) = max{poids.(u) + f(l-1, u, t)| u voisin de s}. *)

let poids_chemins (g, poids) = 
    let n = Array.length g in
    let pmax = Array.init n (fun _ -> Array.make_matrix n n neg_infinity) in
    for s = 0 to n - 1 do 
        let traiter t =
            pmax.(0).(s).(t) <- float_of_int poids.(t) in
        List.iter traiter g.(s)
    done;
    for l = 1 to n - 1 do
        for s = 0 to n - 1 do
            for t = 0 to n - 1 do
                let traiter u =
                    pmax.(l).(s).(t) <- max pmax.(l).(s).(t) 
                        (pmax.(0).(s).(u) +. pmax.(l-1).(u).(t)) in
                List.iter traiter g.(s)
            done
        done
    done;
    pmax

let v_infini (h, poids) =
    let n = Array.length h in
    let pmax = poids_chemins (h, poids) in
    let vmax = ref 0. in
    let foi = float_of_int in
    for s = 0 to n - 1 do
        for l = 1 to n - 1 do
            vmax := max !vmax (pmax.(l).(s).(s) /. (foi (l + 1)))
        done
    done;
    !vmax

let q11 u0 =
    let traiter (n, m, p) =
        let h, poids = graphe_h n m p u0 in
        let maxv = v_infini (h, poids) in
        Printf.printf "Pour n, m, p = %d, %d, %d, la valeur max vaut %f\n" 
            n m p maxv in
    List.iter traiter [(6, 3, 10); (10, 3, 10); 
                       (40, 4, 10); (99, 7, 10)]  

(* Tests *)

let main = 
    let u0 = 104 in
    q1 u0;
    q2 u0;
    q3 u0;
    q4 u0;
    q5 u0;
    q6 u0;
    q7 u0; 
    q8 u0;
    q9 u0;
    q10 u0;
    q11 u0;