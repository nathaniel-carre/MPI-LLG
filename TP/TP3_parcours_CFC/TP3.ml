(* Les tests dans ce fichiers sont prévus pour une interprétation
(et non une compilation) du fichier : en particulier, on n'a pas
écrit de fonction d'affichage de tableau/listes. Les tests sont à la fin
du fichier. *)

type graphe = int list array

let g1 = [| [6; 8]; [5]; [3; 8]; [2]; [8];
            [1]; [0; 8]; []; [0; 2; 4; 6] |]

let g2 = [| [4; 5]; [2; 3; 6]; [7]; [2; 9];
            [3; 5]; []; [8]; [2]; [1; 6]; [0] |]

let g3 = [| [2; 9]; [8]; [0; 5]; [7]; [9]; [2]; 
            [8]; [3]; [1; 6]; [0; 4; 10]; [9] |]

let g4 = [| [7]; [4]; [0; 6]; [1; 2; 5]; 
            [7]; [4]; [7]; [] |] 

let composantes_connexes g =
    (* On crée un tableau des sommets vus, et une fonction locale
    de DFS, qu'on applique sur chaque sommet. On utilise des références
    de listes pour remplir le résultat. *)
    let n = Array.length g in
    let vus = Array.make n false in
    let lcc = ref [] and cc = ref [] in
    let rec dfs s = 
        if not vus.(s) then begin
            vus.(s) <- true;
            cc := s :: !cc;
            List.iter dfs g.(s)
        end in            
    for s = 0 to n - 1 do
        if not vus.(s) then 
           (dfs s; lcc := !cc :: !lcc; cc := [])
    done;
    !lcc

let acyclique_no g =
    (* Il suffit de vérifier que |A| = |S| - m, avec m le nombre
    de composantes connexes. *)
    let somme_degres = Array.fold_left (fun d lst -> d + List.length lst) 0 g in
    let lcc = composantes_connexes g in
    somme_degres / 2 = Array.length g - List.length lcc

let numeros_DFS g =
    (* On applique tel quel l'algorithme donné dans le cours. *)
    let n = Array.length g in
    let pre = Array.make n (-1) and
        post = Array.make n (-1) in
    let k = ref 0 in
    let rec dfs s =
        if pre.(s) = -1 then begin
           incr k;
           pre.(s) <- !k;
           List.iter dfs g.(s);
           incr k;
           post.(s) <- !k
        end in
    for s = 0 to n - 1 do
        if pre.(s) = -1 then dfs s
    done;
    pre, post

exception Cyclique

let acyclique_o g =
    (* On vérifie pour chaque arête (s,t) que post(s) > post(t). 
    On utilise une levée d'exception pour arrêter la vérification
    si on trouve une contradiction. *)
    let n = Array.length g in
    let _, post = numeros_DFS g in
    try for s = 0 to n - 1 do
            let test t =
                if post.(s) < post.(t) then
                raise Cyclique in
            List.iter test g.(s)
        done;
        true
    with Cyclique -> false

let ordre_topo g =
    (* Dans cette version, on convertit le tableau des numéros
    postfixes en un ordre postfixe. Pour ce faire, on utilise un
    tableau intermédiaire dont les indices sont les différents
    instants (de 1 à 2n), et on ne garde que les instants correspondant
    à un numéro postfixe. *)
    let n = Array.length g in
    let _, post = numeros_DFS g in
    let ordre = Array.make (2 * n + 1) (-1) in
    for s = 0 to n - 1 do
        ordre.(post.(s)) <- s
    done;
    let topo = Array.make n (-1) in
    let s = ref 0 in
    for i = 2 * n downto 1 do
        if ordre.(i) <> -1 then
           (topo.(!s) <- ordre.(i); incr s)
    done;
    topo

let ordre_topo g =
    (* Dans cette version, on reprend le schéma du parcours, mais
    en remplissant une liste plutôt qu'en modifiant les numéros. *)
    let n = Array.length g in
    let vus = Array.make n false in
    let topo = ref [] in
    let rec dfs s =
        if not vus.(s) then begin
           vus.(s) <- true;
           List.iter dfs g.(s);
           topo := s :: !topo
        end in
    for s = 0 to n - 1 do
        if not vus.(s) then dfs s
    done;
    Array.of_list !topo

let transpose g =
    (* On parcourt chaque liste d'adjacence et on crée une arête
    inversée pour chaque sommet vu. *)
    let n = Array.length g in
    let gt = Array.make n [] in
    for s = 0 to n - 1 do
        let arete t = gt.(t) <- s :: gt.(t) in
        List.iter arete g.(s)
    done;
    gt

let kosaraju g =
    (* On applique l'algorithme vu en cours. On parcourt les sommets
    selon l'ordre inverse d'un parcours postfixe du graphe transposé, 
    et lorsqu'on tombe sur un sommet non vu, on lance un parcours, ce
    qui permet d'atteindre toute la CFC du sommet. *)
    let n = Array.length g in
    let gt = transpose g in
    let topo = ordre_topo gt in
    let vus = Array.make n false in
    let lcfc = ref [] and cfc = ref [] in    
    let rec dfs s = 
        if not vus.(s) then begin
            vus.(s) <- true;
            cfc := s :: !cfc;
            List.iter dfs g.(s)
        end in            
    for s = 0 to n - 1 do
        if not vus.(topo.(s)) then 
           (dfs topo.(s); lcfc := !cfc :: !lcfc; cfc := [])
    done;
    !lcfc

type litteral = Pos of int | Neg of int
type clause = litteral * litteral
type deux_cnf = clause list

let phi0 = [(Neg 1, Neg 2); (Neg 0, Pos 2); (Pos 2, Pos 2); (Pos 1, Pos 0); (Neg 1, Pos 2)]
let phi1 = [(Neg 0, Pos 3); (Pos 0, Neg 2); (Pos 1, Neg 2); (Pos 0, Pos 2); (Neg 1, Neg 0); (Neg 3, Pos 2)]

let eval_litt mu = function
    | Neg i -> 1 - mu.(i)
    | Pos i -> mu.(i)

let evaluer mu phi = 
    (* On calcule le minimum des évaluations des clauses, c'est-à-dire le 
    minimum des maximums des littéraux des clauses. *)
    List.fold_left (fun a (x, y) -> min a (max (eval_litt mu x) (eval_litt mu y))) 1 phi

let incremente mu =
    (* On part de la fin, on remplace les 1 consécutifs par des 0, puis
    si on n'a pas tout remplacé, on remplace le dernier 0 par un 1. On renvoie
    le booléen en conséquence. *)
    let n = Array.length mu in
    let i = ref (n - 1) in
    while !i >= 0 && mu.(!i) = 1 do
        mu.(!i) <- 0;
        decr i
    done;
    if !i >= 0 then (mu.(!i) <- 1; true)
    else false

let modele n phi =
    (* En commençant par un modèle qui évalue chaque variable à zéro,
    on teste s'il satisfait la formule et on incrémente, jusqu'à ce
    qu'on tombe sur un modèle, ou qu'on ait tout testé. *)
    let mu = Array.make n 0 in
    while evaluer mu phi = 0 && incremente mu do () done;
    if evaluer mu phi = 1 then Some mu else None

let graphe_implication n phi =
    (* On transforme chaque clause l1 v l2 en deux arêtes ¬l1 → l2 
    et ¬l2 → l1. Les deux fonctions auxiliaires permettent de calculer
    le numéro de sommet d'un littéral ou de sa négation. *)
    let g = Array.make (2 * n) [] in
    let pos = function
        | Neg i -> i + n
        | Pos i -> i
    and neg = function
        | Neg i -> i
        | Pos i -> i + n
    in
    List.iter (
        fun (x, y) -> 
            g.(neg x) <- pos y :: g.(neg x);
            g.(neg y) <- pos x :: g.(neg y)
        )
        phi;
    g

let convertir k lcfc =
    (* On utilise une référence pour garder en mémoire le numéro de
    cfc. On parcourt chaque sous-liste pour numéroter les sommets. *)
    let cfc = Array.make k (-1) in
    let i = ref 0 in
    List.iter (
        fun lst ->
            List.iter (fun s -> cfc.(s) <- !i) lst;
            incr i
        ) 
        lcfc;
    cfc

exception Unsat

let deux_sat n phi =
    (* On cherche si une variable et sa négation sont dans la même cfc. *)
    let g = graphe_implication n phi in
    let lcfc = kosaraju g in
    let cfc = convertir (2 * n) lcfc in
    try for i = 0 to n - 1 do
            if cfc.(i) = cfc.(i + n) then raise Unsat
        done;
        true
    with Unsat -> false

let modele2 n phi =
    (* On parcourt les CFC dans l'ordre topologique, et on
    attribue la valeur 0 aux littéraux vus pour la première fois.
    On pense également à vérifier si une variable et sa négation
    sont dans la même CFC. *)
    let g = graphe_implication n phi in
    let lcfc = kosaraju g in
    let cfc = convertir (2 * n) lcfc in
    let mu = Array.make n (-1) in
    try List.iter (
            List.iter (
                fun s ->
                    if s < n then begin
                        if cfc.(s) = cfc.(s + n) then raise Unsat;
                        if mu.(s) = -1 then mu.(s) <- 0
                    end else begin
                        if cfc.(s) = cfc.(s - n) then raise Unsat;
                        if mu.(s - n) = -1 then mu.(s - n) <- 1
                    end
            )
        )
        lcfc;
        Some mu
    with Unsat -> None

let lire_formule str =
    let lire_litt litt =
        let n = String.length litt in
        let i = int_of_string (String.sub litt 1 (n - 1)) in
        if litt.[0] = '+' then Pos i
        else Neg i
    in
    let lire_clause clause =
        match String.split_on_char ',' clause with
            | [litt1; litt2] -> (lire_litt litt1, lire_litt litt2)
            | _ -> assert false
    in
    List.map lire_clause (String.split_on_char ';' str)

let lire_fichier fichier =
    let ic = open_in fichier in
    let formules = ref [] in
    (try while true do
            let ligne = input_line ic in
            match String.split_on_char ' ' ligne with
                | [n; f] -> formules := (int_of_string n, lire_formule f) :: !formules
                | _ -> assert false
        done
    with _ -> close_in ic);
    List.rev !formules

let rec verif_sat = function
    | [] -> true
    | (n, phi) :: lst -> deux_sat n phi && verif_sat lst

let rec verif_unsat = function
    | [] -> true
    | (n, phi) :: lst -> not (deux_sat n phi) && verif_unsat lst

let rec verif_sat2 = function
    | [] -> true
    | (n, phi) :: lst -> begin match modele2 n phi with
        | None -> false
        | Some mu -> evaluer mu phi = 1 && verif_sat2 lst
    end

let rec verif_unsat2 = function
    | [] -> true
    | (n, phi) :: lst ->  modele2 n phi = None && verif_unsat2 lst

let _ = composantes_connexes g1
let _ = composantes_connexes g3
let _ = 
    assert (not (acyclique_no g1));
    assert (acyclique_no g3)
let _ = numeros_DFS g2
let _ = numeros_DFS g4
let _ =
    assert (not (acyclique_o g2));
    assert (acyclique_o g4)
let _ = ordre_topo g2
let _ = ordre_topo g4
let _ = transpose g2
let _ = transpose g4
let _ = kosaraju g2
let _ = kosaraju g4
let _ = evaluer [|1;0;1|] phi0
let _ = evaluer [|1;0;0;1|] phi1
let _ = modele 3 phi0
let _ = modele 4 phi1
let _ =
    assert (deux_sat 3 phi0);
    assert (not (deux_sat 4 phi1))
let _ = modele2 3 phi0
let _ = modele2 4 phi1
let _ = 
    assert (verif_sat (lire_fichier "satisfiables.txt"));
    assert (verif_unsat (lire_fichier "insatisfiables.txt"));
    assert (verif_sat2 (lire_fichier "satisfiables.txt"));
    assert (verif_unsat2 (lire_fichier "insatisfiables.txt"))

