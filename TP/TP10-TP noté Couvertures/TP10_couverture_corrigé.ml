(* Question 1 *)
let tab_u u0 n =
    (* Crée un tableau qui contient les n + 1 premières valeurs de la suite. *)
    let u = Array.make (n + 1) u0 in
    for i = 1 to n do
        u.(i) <- (15_731 * u.(i - 1)) mod 32_003
    done;
    u;;

let q1 u0 =
    let traiter n =
        Printf.printf "u_%d = %d\n" n (tab_u u0 n).(n) in
    List.iter traiter [1_000; 10_000; 30_000];;

(* Question 2 *)
let v u i j = u.(1 + i + (j * (j - 1)) / 2);;

let creer_graphe m n u0 =
    (* On suit le principe de création du graphe. On suppose i < j
       pour éviter de créer des arêtes en doublon. *)
    let u = tab_u u0 (1 + (n * (n + 1)) / 2) in
    let g = Array.make n [] in
    for i = 0 to n - 1 do
        for j = i + 1 to n - 1 do
            if v u i j mod m = 0 then begin
                g.(i) <- j :: g.(i);
                g.(j) <- i :: g.(j)
            end
        done
    done;
    g;;

(* Calcule le tableau des degrés. *)
let degres g = Array.map List.length g;;

(* Calcule la somme des degrés. *)
let degre g = Array.fold_left max 0 (degres g);;

(* Calcule le nombre d'arêtes. *)
let nb_aretes g = (Array.fold_left (+) 0 (degres g)) / 2;;

let q2 u0 =
    let traiter (m, n) =
        let g = creer_graphe m n u0 in
        Printf.printf 
            "Pour m = %d, n = %d, le nombre d'arêtes est %d et le degré est %d\n"
            m n (nb_aretes g) (degre g) in
    List.iter traiter [5, 10; 10, 50; 50, 250];;

(* Question 3 *)
let sommets g =
    (* Renvoie le tableau des sommets, par ordre croissant de la relation. *)
    let som = Array.mapi (fun i x -> - (List.length x), i) g in
    Array.sort compare som;
    Array.map snd som;;

let sommet_min g = (sommets g).(0);;

let sommet_max g = (sommets g).(Array.length g - 1);;

let q3 u0 =
    let traiter (m, n) =
        let g = creer_graphe m n u0 in
        Printf.printf 
            "Pour m = %d, n = %d, sommet min est %d et le sommet max est %d\n"
            m n (sommet_min g) (sommet_max g) in
    List.iter traiter [5, 10; 10, 50; 50, 250];;

(* Question 4 *)

(* On représente un sous-ensemble X des sommets dans G = (S, A) par un tableau couv de 
   n = |S| booléens, tels que couv.(s) vaut true si et seulement si s ∈ X. *)

let est_couverture g couv =
    (* Détermine si un ensemble est une couverture. On vérifie s'il existe une arête non
       couverte en les parcourant toutes. *)
    let n = Array.length g in
    try for s = 0 to n - 1 do
        if not (couv.(s)) then 
            if List.exists (fun t -> not couv.(t)) g.(s) then
                failwith "erreur"
        done;
        true
    with _ -> false;;

let suivante couv =
    (* Calcule la couverture suivante selon l'ordre des couvertures donné par l'énoncé.
       Pour faciliter l'explication, on assimile true à 1 et false à 0. *)
    let n = Array.length couv in
    (* On commence par remplacer la plus longue séquence de 1 de la fin par des 0. *)
    let i = ref (n - 1) in
    while !i >= 0 && couv.(!i) do 
        couv.(!i) <- false;
        decr i;        
    done;
    (* Ensuite, on détermine la plus longue séquence de 0 qui précède ces 1. *)
    let j = ref !i in
    while !j >= 0 && not couv.(!j) do decr j done;
    (* Le principe est le suivant :
        - si n - 1 n'est pas dans la couverture, on enlève j et on rajoute j + 1
        - sinon, pour passer à la couverture suivante, on enlève j, on ajoute j + 1 et on rajoute 
          à la suite de j + 1 autant d'éléments qu'il y en avait déjà plus grands que j. *)
    if !j >= 0 then couv.(!j) <- false;
    if !i >= 0 then 
        for k = 0 to n - 1 - !i do
            couv.(!j + 1 + k) <- true
        done;
    !i >= 0;;

exception Trouve;;

let couverture_min g =
    (* Calcule la couverture minimale en les parcourant toutes, dans l'ordre. *)
    let n = Array.length g in
    let couv = Array.make n false in
    try while true do
            if est_couverture g couv then raise Trouve;
            ignore (suivante couv)
        done; 
        couv
    with _ -> couv;;

let afficher_ensemble couv =
    (* Affiche un ensemble. *)
    let n = Array.length couv in
    for i = 0 to n - 1 do
        if couv.(i) then Printf.printf "%d; " i
    done;
    print_newline ();;

let q4 u0 =
    let traiter (m, n) =
        let g = creer_graphe m n u0 in
        Printf.printf "Pour m = %d, n = %d, la couverture minimale est " m n;
        afficher_ensemble (couverture_min g)
    in
    List.iter traiter [5, 10; 6, 16; 7, 20];;

(* Question 5 *)
let composantes g =
    (* Un calcul classique des composantes connexes. La fonction renvoie un tableau
       comp tel que pour s ∈ S, comp.(s) est égal au numéro de la composante connexe
       de s. Elle renvoie également le nombre de composantes, et le cardinal maximal
       d'une composante. *)
    let n = Array.length g in
    let k = ref 0 and card = ref 0 and card_max = ref 0 in
    let comp = Array.make n (-1) in
    (* On écrit un DFS récursif. *)
    let rec dfs s =
        if comp.(s) = - 1 then begin
            comp.(s) <- !k;
            incr card;
            List.iter dfs g.(s)
        end in
    (* On lance le DFS depuis chaque sommet pas encore vu. *)
    for s = 0 to n - 1 do
        if comp.(s) = - 1 then begin
            card := 0;
            dfs s;
            if !card > !card_max then card_max := !card;
            incr k
        end
    done;
    comp, !k, !card_max;;

let q5 u0 =
    let traiter (m, n) =
        let g = creer_graphe m n u0 in
        let _, k, cm = composantes g in
        Printf.printf 
            "Pour m = %d, n = %d, le nombre de composantes est %d et
\t la plus grande composante est de cardinal %d\n"
            m n k cm in
    List.iter traiter [5, 10; 6, 16; 7, 20; 10, 50; 
                       50, 50; 125, 100; 50, 250; 360, 250];;

(* Question 6 *)
let ieme_composante g comp i =
    (* Construit le sous-graphe qui correspond à la ième composante, en
       prenant en argument le tableau comp calculé avec la fonction composantes. *)
    let n = Array.length g in
    let n' = ref 0 in
    (* On crée un tableau de renumérotation des sommets. *)
    let numero = Array.make n (-1) in
    for s = 0 to n - 1 do
        if comp.(s) = i then begin
            numero.(s) <- !n';
            incr n'
        end
    done;
    let gi = Array.make !n' [] in
    for s = 0 to n - 1 do
        if comp.(s) = i then
            gi.(numero.(s)) <- List.map (fun t -> numero.(t)) g.(s)
    done;
    gi;;

let cardinal couv =
    (* Calcule le cardinal d'un ensemble (le nombre de true). *)
    let n = Array.length couv in
    let card = ref 0 in
    for i = 0 to n - 1 do
        if couv.(i) then incr card
    done;
    !card;;

let couverture_cc g =
    (* Calcule le cardinal d'une couverture en faisant le travail composante par
       composante. *)
    let comp, k, _ = composantes g in
    let card = ref 0 in
    for i = 0 to k - 1 do
        let gi = ieme_composante g comp i in
        card := !card + cardinal (couverture_min gi)
    done;
    !card;;

let q6 u0 =
    let traiter (m, n) =
        let g = creer_graphe m n u0 in
        Printf.printf 
            "Pour m = %d, n = %d, le cardinal de la couverture est %d\n"
            m n (couverture_cc g) in
    List.iter traiter [50, 50; 125, 100; 360, 250];;

(* Question 7 *)
let couverture_approx g =
    (* Applique l'algorithme d'approximation. On parcourt les sommets par ordre
       croissant, et on garde un sommet seulement s'il est relié à un sommet qui
       n'a pas encore été gardé (c'est-à-dire un sommet plus grand). *)
    let n = Array.length g in
    let couv = Array.make n false in
    for s = 0 to n - 1 do
        if List.exists (fun t -> not couv.(t)) g.(s) then
            couv.(s) <- true;
    done;
    couv;;

let q7 u0 =
    let traiter (m, n) =
        let g = creer_graphe m n u0 in
        Printf.printf 
            "Pour m = %d, n = %d, le cardinal de la couverture approchée est %d\n"
            m n (cardinal (couverture_approx g)) in
    List.iter traiter [5, 10; 6, 16; 7, 20; 10, 50; 
                       50, 50; 125, 100; 50, 250; 360, 250];;

(* Question 8 *)

(* Comme les arêtes disparaissent au fur et à mesure, on ne peut pas utiliser la même méthode
   qu'en Q7 en parcourant les sommets par ordre de la relation. *)

let sommet_min g couv =
    (* Calcule le sommet non couvert minimal pour la relation. On calcule un « degré » correspondant
       au nombre de voisins non couverts. *)
    let n = Array.length g in
    let degre s = List.fold_left (fun x t -> x + if not couv.(t) then 1 else 0) 0 g.(s) in
    let degres = Array.init n degre in
    let smin = ref 0 in
    for s = 1 to n - 1 do
        if not couv.(s) && (couv.(!smin) || degres.(s) > degres.(!smin)) then smin := s
    done;
    !smin;;

exception Fini;;

let couverture_approx2 g =
    (* Tant qu'il reste un sommet qui a un voisin non couvert, on ajoute celui qui est minimal. 
       On renvoie la couverture sous forme de tableau de booléens et de liste, pour la question 9. *)
    let n = Array.length g in
    let som = sommets g in
    let couv = Array.make n false in
    let degre s = List.fold_left (fun x t -> x + if not couv.(t) then 1 else 0) 0 g.(s) in
    let couv_liste = ref [] in
    try while true do
            let smin = sommet_min g couv in
            if degre smin = 0 || couv.(smin) then raise Fini;
            couv.(smin) <- true;
            couv_liste := smin :: !couv_liste
        done;
        couv, !couv_liste
    with _ -> couv, !couv_liste;;

let q8 u0 =
    let traiter (m, n) =
        let g = creer_graphe m n u0 in
        Printf.printf 
            "Pour m = %d, n = %d, le cardinal de la 2e couverture approchée est %d\n"
            m n (cardinal (fst (couverture_approx2 g))) in
    List.iter traiter [5, 10; 6, 16; 7, 20; 10, 50; 
                       50, 50; 125, 100; 50, 250; 360, 250];;

(* Question 9 *)

(* À noter, il y a une erreur d'énoncé : avec l'algorithme qui est (vaguement) décrit, 
   la couverture obtenue n'est pas minimale. On propose deux versions ici : une première
   version permet d'obtenir les mêmes résultats que ceux initialement dans l'énoncé (avec
   des couvertures non minimales). La deuxième version donne les résultats de couvertures 
   minimales pour l'inclusion, correspondant à la correction annotée. *)

let couverture_approx3 g =
    (* On parcourt les sommets dans l'ordre inverse où ils ont été ajouté à l'algorithme
       précédent, et on ne garde que ceux qui ont un voisin non couvert. *)
    let n = Array.length g in
    let couv = Array.make n false in
    let couv2 = snd (couverture_approx2 g) in
    let traiter s =
        if List.exists (fun t -> not couv.(t)) g.(s) then
            couv.(s) <- true in
    List.iter traiter couv2;
    couv;;

let q9 u0 =
    let traiter (m, n) =
        let g = creer_graphe m n u0 in
        Printf.printf 
            "Pour m = %d, n = %d, le cardinal de la 3e couverture approchée est %d\n"
            m n (cardinal (couverture_approx3 g)) in
    List.iter traiter [5, 10; 6, 16; 7, 20; 10, 50; 
                       50, 50; 125, 100; 50, 250; 360, 250];;

let couverture_approx3bis g =
    (* On parcourt les sommets dans l'ordre inverse où ils ont été ajouté à l'algorithme
       précédent, et on supprime ceux qui peuvent l'être. *)
    let couv, couv2 = couverture_approx2 g in
    let traiter s =
        if List.for_all (fun t -> couv.(t)) g.(s) then couv.(s) <- false in
    List.iter traiter couv2;
    couv;;

let q9bis u0 =
    let traiter (m, n) =
        let g = creer_graphe m n u0 in
        Printf.printf 
            "Pour m = %d, n = %d, le cardinal de la 3e couverture approchée est %d\n"
            m n (cardinal (couverture_approx3bis g)) in
    List.iter traiter [5, 10; 6, 16; 7, 20; 10, 50; 
                       50, 50; 125, 100; 50, 250; 360, 250];;