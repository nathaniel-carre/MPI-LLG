(* Question 1 *)

let rec u u0 = function
    (* Fonction récursive qui calcule le n-ème terme.
       Attention, pour éviter un stack overflow, il
       faut faire une fonction récursive terminale, ou
       bien faire le calcul avec une boucle. *)
    | 0 -> u0
    | n -> u ((u0 * 19_999_999) mod 19_999_981) (n - 1)

open Printf;;

let q1 u0 =
    (* Pour chaque question, il est recommendé d'écrire
       une fonction qui ne prend que le u0 en argument,
       pour pouvoir changer rapidement entre le u0 de
       référence et le u0 personnel *)
    printf "Question 1\n";
    let traiter i = printf "%d\n" (u u0 i mod 1000) in
    List.iter traiter [123; 456_000; 789_000_000]

(* Question 2 *)

let tableau_u u0 n =
    (* On crée une fonction qui renvoie un tableau
       contenant les n+1 premières valeurs de la suite,
       plutôt qu'une fonction qui calcule le n-ème
       terme *)
    let tab = Array.make (n + 1) u0 in
    for i = 1 to n do
        tab.(i) <- (19_999_999 * tab.(i-1)) mod 19_999_981
    done;
    tab

let somme tab =
    (* Cette fonction calcule la somme modulo 1000 des
       termes d'un tableau. On applique le modulo tout
       en sommant pour éviter des débordements. *)
    Array.fold_left (fun a b -> (a + b) mod 1000) 0 tab

let q2 u0 =
    printf "Question 2\n";
    let traiter m =
        let tab = tableau_u u0 m in
        Printf.printf "%d\n" (somme tab)
    in
    List.iter traiter [123; 45_600; 78_900_000]

(* Question 3 *)

let graphe_matrice u0 n p a =
    (* Cette fonction renvoie la matrice d'adjacence
       du graphe. Cela permet d'éviter d'avoir à gérer
       les doublons d'arêtes. *)
    let mat = Array.make_matrix n n 0 in
    let tab = tableau_u u0 (a + 2 * p) in
    for i = 0 to p - 1 do
        let s = tab.(a + i) mod n and
            t = tab.(a + i + p) mod n in
        if s <> t then begin 
            mat.(s).(t) <- 1;
            mat.(t).(s) <- 1
        end
    done;
    mat

let degre mg s =
    (* Calcule le degré du sommet s dans le graphe g
       donné par matrice d'adjacence. *)
    Array.fold_left (+) 0 mg.(s)

let degre_max mg =
    (* Calcule le degré maximal d'un sommet dans le graphe
       g donné par matrice d'adjacence. *)
    let dmax = ref 0 in
    for s = 0 to Array.length mg - 1 do
        dmax := max !dmax (degre mg s)
    done;
    !dmax

let q3 u0 =
    printf "Question 3\n";
    let traiter (n, p, a) =
        let g = graphe_matrice u0 n p a in
        let deg = degre_max g in
        Printf.printf "%d\n" deg
    in
    List.iter traiter [100, 300, 123; 1_000, 5_000, 456; 10_000, 50_000_000, 789]

(* Question 4 *)

let graphe u0 n p a =
    (* Pour faire des parcours et des calculs de chemins,
       il est plus simple de travailler avec une représentation
       par tableau de listes d'adjacence. *)
    let g = Array.make n [] in
    let tab = tableau_u u0 (a + 2 * p) in
    for i = 0 to p - 1 do
        let s = tab.(a + i) mod n and
            t = tab.(a + i + p) mod n in
        if not (List.mem s g.(t)) && s <> t then begin
            g.(s) <- t :: g.(s);
            g.(t) <- s :: g.(t)
        end
    done;
    g

let composante g s =
    (* On implémente ici un parcours de graphe pour déterminer
       le nombre de sommets dans la composante de 0. On travaille
       sans récursivité pour éviter le dépassement de pile. *)
    let n = Array.length g in
    let vus = Array.make n false in
    let nb = ref 0 in
    let pile = Stack.create () in
    Stack.push s pile;
    while not (Stack.is_empty pile) do
        let t = Stack.pop pile in
        if not vus.(t) then begin
            vus.(t) <- true;
            incr nb;
            List.iter (fun u -> Stack.push u pile) g.(t)
        end
    done;
    !nb

let q4 u0 =
    printf "Question 4\n";
    let traiter (n, p, a) =
        let g = graphe u0 n p a in
        printf "%d\n" (composante g 0) 
    in
    List.iter traiter [(100, 90, 123); (10_000, 9_000, 456); (500_000, 450_000, 789)]

(* Question 5 *)
let composantes g =
    (* Même idée que q4, sauf qu'on lance un parcours
       depuis chaque nouveau sommet non vu. On renvoie
       ici la liste des composantes connexes. *)
    let n = Array.length g in
    let vus = Array.make n false in
    let lcc = ref [] and cc = ref [] in
    for s = 0 to n - 1 do
        if not vus.(s) then begin
            let pile = Stack.create () in
            Stack.push s pile;
            while not (Stack.is_empty pile) do
                let t = Stack.pop pile in
                if not vus.(t) then begin
                    vus.(t) <- true;
                    cc := t :: !cc;
                    List.iter (fun u -> Stack.push u pile) g.(t)
                end
            done;
            lcc := !cc :: !lcc;
            cc := []
        end
    done;
    !lcc;;

let q5 u0 =
    printf "Question 5\n";
    let traiter (n, p, a) =
        let g = graphe u0 n p a in
        printf "%d\n" (List.length (composantes g)) 
    in
    List.iter traiter [(100, 90, 123); (10_000, 9_000, 456); (500_000, 450_000, 789)]

(* Question 6 *)

let mini lst =
    (* Calcule le minimum d'une liste. *)
    List.fold_left min (List.hd lst) lst

let maxi lst =
    (* Calcule le maximum d'une liste. *)
    List.fold_left max (List.hd lst) lst

let graphe_connexe u0 n p a =
    (* Calcule le graphe mis à jour avec les arêtes 
       rajoutées pour le rendre connexe. On utilise
       List.rev_map car List.map n'est pas récursive
       terminale. *)
    let g = graphe u0 n p a in
    let lcc = composantes g in
    let nlcc = List.rev_map (fun lst -> mini lst, maxi lst) lcc in
    let sommets = List.sort compare nlcc in
    let rec ajout_aretes = function
        | [] -> assert false
        | [_] -> ()
        | (_, s) :: (t, u) :: q ->
            g.(s) <- t :: g.(s);
            g.(t) <- s :: g.(t);
            ajout_aretes ((t, u) :: q)
    in
    ajout_aretes sommets;
    g

let distances g s =
    (* Calcule les distances d'un sommet s à tous les
       autres à l'aide d'un parcours en largeur. *)
    let dist = Array.make (Array.length g) (-1) in
    let file = Queue.create () in
    dist.(s) <- 0;
    Queue.add s file;
    while not (Queue.is_empty file) do
        let t = Queue.take file in
        let traiter u =
            if dist.(u) = -1 then begin
                dist.(u) <- dist.(t) + 1;
                Queue.add u file
            end
        in
        List.iter traiter g.(t)
    done;
    dist

let q6 u0 =
    printf "Question 6\n";
    let traiter (n, p, a) =
        let g = graphe_connexe u0 n p a in
        let dist = distances g 0 in
        printf "%d\n" dist.(n - 1) 
    in
    List.iter traiter [(100, 90, 123); (10_000, 9_000, 456); (500_000, 450_000, 789)]

(* Question 7 *)

let rec plus_proche dist = function
    (* Étant donné un tableau de distances et une liste de
       sommets, renvoie le sommet de la liste dont la distance 
       est minimale. *)
    | [] -> assert false
    | [s] -> s
    | s :: q -> 
        let t = plus_proche dist q in
        if dist.(s) < dist.(t) then s else t

let glouton g k =
    (* Renvoie le nombre de mouvements nécessaires pour ramasser
       k pièces sur le graphe g avec la stratégie gloutonne. *)
    let n = Array.length g in
    let rec recup depart pieces =
        (* Cette fonction renvoie le nombre de déplacements à
           effectuer pour récupérer les pièces données par la
           liste pieces, en partant du sommet depart. *)
        if pieces = [] then 0
        else begin
            let dist = distances g depart in
            let s = plus_proche dist pieces in
            let reste = List.filter ((<>) s) pieces in
            dist.(s) + recup s reste
        end
    in
    recup 0 (List.init k (fun i -> n - 1 - i))

let q7 u0 =
    printf "Question 7\n";
    let traiter (n, p, a, k) =
        let g = graphe_connexe u0 n p a in
        printf "%d\n" (glouton g k) 
    in
    List.iter traiter [(100, 100, 123, 8); (10_000, 5_000, 456, 50); (500_000, 450_000, 789, 18)]

(* Question 8 *)

(* On va utiliser un algorithme de programmation dynamique, nécessaire pour pouvoir calculer
   la réponse pour le (c). L'idée est la suivante : pour X ⊂ S, et s ∈ S, on note D(s, X) le
   nombre minimal de déplacements à faire pour passer par tous les sommets de X en partant de s.
   On a alors :
     - D(s, ∅) = 0
     - D(s, X) = min{d(s, t) + D(t, X\{t}) | t ∈ X} : on envisage chaque sommet de X comme premier
       sommet à aller visiter.
   Pour garder en mémoire les résultats déjà calculés, on utilise une table de hachage. Problème :
   il faut utiliser des clés pouvant être hachées. On choisit ici des listes, qui sont des objets
   non mutables. On les garde triées pour ne pas risquer de modifier le haché. *)

let rec calcul_D dists s lstX ht =
    (* - ht est la table de hachage qui garde en mémoire les
       D(s, X) déjà calculés.
       - dists est un tableau de taille k + 1 tel que dists.(0) est le tableau
       des distances au sommet 0 et pour i > 0, dists.(i) est le tableau
       des distances au sommet n - i (puisqu'il n'y aura besoin de calculer
       les distances que depuis les sommets 0, n - k, n - k + 1, …, n - 1). *)
    if not (Hashtbl.mem ht (s, lstX)) then begin
        if lstX = [] then Hashtbl.add ht (s, []) 0
        else             
            let n = Array.length dists.(0) in
            let dist = if s = 0 then dists.(0) else dists.(n - s) in
            let traiter t = 
                let reste = List.filter ((<>) t) lstX in
                dist.(t) + calcul_D dists t reste ht
            in
            Hashtbl.add ht (s, lstX) (mini (List.map traiter lstX))
    end;
    Hashtbl.find ht (s, lstX)

let optimal g k =
    (* Renvoie le nombre de déplacement minimal pour
       récupérer k pièces dans g. *)
    let ht = Hashtbl.create 1 in
    let n = Array.length g in
    let dists = Array.make (k + 1) [||] in
    dists.(0) <- distances g 0;
    for i = 1 to k do
        dists.(i) <- distances g (n - i)
    done;
    calcul_D dists 0 (List.init k (fun i -> n - i - 1)) ht

let q8 u0 =
    printf "Question 8\n";
    let traiter (n, p, a, k) =
        let g = graphe_connexe u0 n p a in
        printf "%d\n" (optimal g k) 
    in
    List.iter traiter [(100, 100, 123, 8); (10_000, 5_000, 456, 10); (500_000, 450_000, 789, 18)]

(* Tests *)

let test_jusqua i u0 =
    (* Permet de faire les tests avec toutes les questions jusqu'à un certain indice. *)
    let questions = [|q1; q2; q3; q4; q5; q6; q7; q8|] in
    for q = 1 to i do
        questions.(q - 1) u0
    done
