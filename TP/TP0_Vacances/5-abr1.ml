type abr =
    | Vide
    | N of abr * int * abr

let rec inserer x = function
    (* On compare à la racine pour savoir si on insère à
       gauche ou à droite. On pense à reconstruire l'arbre
       après l'appel récursif. *)
    | Vide -> N (Vide, x, Vide)
    | N (g, y, d) ->
        if x < y then N (inserer x g, y, d)
        else N (g, y, inserer x d)

let rec extraire_min = function
    (* Extrait le minimum d'un ABR, en renvoyant sa valeur
       et l'ABR résultant de sa suppression. Il suffit d'aller
       à gauche à chaque nœud (on s'arrête quand il n'y a plus
       d'enfant gauche). *)
    | Vide -> assert false
    | N (Vide, x, d) -> x, d
    | N (g, x, d) ->
        let x', g' = extraire_min g in
        x', N (g', x, d)

let rec supprimer x = function
    (* Lorsqu'on trouve le nœud, si l'enfant droit est vide, 
       on remplace le nœud par l'enfant gauche. Sinon, on 
       extrait le min à droite, qu'on utilise comme nouvelle
       racine.
       Pour trouver le nœud, on procède comme l'insertion. *)
    | Vide -> Vide
    | N (g, y, Vide) when x = y -> g
    | N (g, y, d) when x = y ->
        let x', d' = extraire_min d in
        N (g, x', d')
    | N (g, y, d) ->
        if x < y then N (supprimer x g, y, d)
        else N (g, y, supprimer x d)

let rec infixe = function
    (* Affiche le parcours infixe d'un ABR. *)
    | Vide -> ()
    | N (g, x, d) ->
        infixe g; Printf.printf "%d, " x; infixe d

let _ =
    let tab1 = [|2; 5; 6; 8; 1; 18; 2; 4|] in
    let a = ref Vide in
    for i = 0 to 7 do
        a := inserer tab1.(i) !a;
        infixe !a;
        print_newline ();
    done;
    let tab2 = [|6; 18; 3; 2|] in
    for i = 0 to 3 do
        a := supprimer tab2.(i) !a;
        infixe !a;
        print_newline ();
    done