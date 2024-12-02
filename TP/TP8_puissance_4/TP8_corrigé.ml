#use "TP8_affichage.ml";;

let p4 = {
    grille = [| [|0; 0; 0; 0; 0; 0; 0|];
                [|0; 0; 0; 0; 0; 0; 0|];
                [|0; 0; 1; 0; 0; 0; 0|];
                [|0; 0; 2; 1; 2; 0; 0|];
                [|0; 1; 2; 2; 1; 0; 0|];
                [|2; 1; 1; 1; 2; 2; 0|] |];
    libres = [|5; 4; 2; 3; 3; 5; 6|];
    joueur = 1;
    vides = 28 };;

(* On crée la grille et le tableau de cases libres avant
   de renvoyer le jeu. *)
let creer_p4 () =
    let grille = Array.make_matrix 6 7 0 in
    let libres = Array.make 7 6 in
    {grille; libres; joueur = 1; vides = 42};;

(* Un coup est possible s'il est joué dans les bornes de
   la grille, et qu'il reste des emplacements libres dans
   la colonne. *)
let coup_possible jeu col =
    0 <= col && col < 7 && jeu.libres.(col) > 0;;

(* On commence par s'assurer que le coup est possible. Ensuite,
   on rajoute le pion du joueur courant dans la colonne, on y
   diminue le nombre de cases libre de la colonne et de cases
   vides total, et on change le joueur courant. *)
let jouer_coup jeu col =
    assert (coup_possible jeu col);
    let k = jeu.libres.(col) in
    jeu.libres.(col) <- k - 1;
    jeu.grille.(k - 1).(col) <- jeu.joueur;
    jeu.joueur <- 3 - jeu.joueur;
    jeu.vides <- jeu.vides - 1;;

(* La même chose, mais dans l'autre sens. *)
let annuler_coup jeu col =
    let k = jeu.libres.(col) in
    assert (k < 6);
    jeu.joueur <- 3 - jeu.joueur;
    assert (jeu.grille.(k).(col) = jeu.joueur);
    jeu.vides <- jeu.vides + 1;
    jeu.grille.(k).(col) <- 0;
    jeu.libres.(col) <- k + 1;;

(* Pour vérifier qu'un coup est gagnant, il va falloir vérifier
   des alignements. On gardera pour cela trace de la ligne et de
   la colonne de la case courante, et on utilise la fonction
   suivante pour vérifier qu'on ne sort pas de la grille. *)
let valide (x, y) = 
    0 <= x && x < 6 && 0 <= y && y < 7;;

(* On compte ici la taille maximale d'un alignement commençant
   à la case (lgn, col), dans la direction (dl, dc), qui correspond
   au joueur donné en argument. On s'arrête si on a 4 cases alignées
   ou si on rencontre une case vide ou de l'autre joueur. *)
let nb_cases jeu joueur (lgn, col) (dl, dc) =
    let i = ref 0 in
    let x = ref (lgn + dl) and y = ref (col + dc) in
    while !i < 3 && valide (!x, !y) && jeu.grille.(!x).(!y) = joueur do
        incr i;
        x := !x + dl;
        y := !y + dc
    done;
    !i;;

(* Ce tableau donne les directions possibles (en différentiel
   de lignes et de colonnes). *)
let directions = [|(1,0);(1,1);(0,1);(-1,1);(-1,0);(-1,-1);(0,-1);(1,-1)|];;

(* Dès lors, la fonction pour déterminer si un coup est gagnant
   consiste à tester s'il existe un alignement gagnant avec cette case.
   Pour cela, on compte le nombre de cases égales au joueur de part
   et d'autre du coup, et on vérifie qu'il y en a au moins 4. *)
let coup_gagnant jeu col joueur =
    let max_cases = ref 0 in
    let lgn = jeu.libres.(col) - 1 in
    for dir = 0 to 3 do
        let nb1 = nb_cases jeu joueur (lgn, col) directions.(dir) in
        let nb2 = nb_cases jeu joueur (lgn, col) directions.(dir + 4) in
        max_cases := max !max_cases (nb1 + nb2 + 1)
    done;
    !max_cases >= 4;;

(* On détermine l'ensemble des coups possibles dans une liste. *)
let coups_possibles jeu =
    let lst = ref [] in
    for col = 0 to 6 do
        if coup_possible jeu col then lst := col :: !lst
    done;
    !lst;;

(* La stratégie aléatoire consiste à tirer au hasard un coup
   parmi ceux qui sont possibles. *)
let strategie_alea jeu =
    let tab = Array.of_list (coups_possibles jeu) in
    tab.(Random.int (Array.length tab));;

(* Pour comparer deux stratégies, on crée un jeu et on fait
   alterner les joueurs jusqu'à ce qu'il y ait un coup gagnant
   ou que la grille soit remplie. *)
let comparer strat1 strat2 =
    let jeu = creer_p4 () in
    let gagnant = ref 0 in
    while !gagnant = 0 && jeu.vides > 0 do
        let col = if jeu.joueur = 1 then strat1 jeu
                                    else strat2 jeu in
        if coup_gagnant jeu col jeu.joueur then gagnant := jeu.joueur;
        jouer_coup jeu col;
    done;
    afficher jeu;
    !gagnant;;

comparer strategie_alea strategie_alea;;

let points = [| [|3;4;5;7;5;4;3|];
                [|4;6;8;10;8;6;4|];
                [|5;8;11;13;11;8;5|];
                [|5;8;11;13;11;8;5|];
                [|4;6;8;10;8;6;4|];
                [|3;4;5;7;5;4;3|] |];;

let f n = n mod 2 - n / 2;;

(* Un simple parcours de matrice. *)
let heuristique jeu =
    let h = ref 0 in
    for i = 0 to 5 do
        for j = 0 to 6 do
            h := !h + points.(i).(j) * f (jeu.grille.(i).(j))
        done
    done;
    !h;;

(* Ces deux fonctions calculent le maximum et le minimum d'une
   liste de couples, selon l'ordre lexicographique. Pour une liste
   vide, on choisit de renvoyer un couple sentinelle. *)
let maxi = List.fold_left max (min_int, -1);;

let mini = List.fold_left min (max_int, -1);;

(* Dès lors, pour le min-max, on commence par écrire une fonction auxiliaire
   qui renvoie des couples (score, coup choisi). On procède récursivement : 
   si la profondeur est nulle ou que la partie est terminée, on renvoie 
   l'heuristique de la grille et -1. Sinon, on détermine les coups possibles,
   et pour chaque coup, on le joue, s'il est gagnant, on s'arrête, sinon on
   calcule récursivement son score, qu'on renvoie avec le coup. On détermine
   ensuite le min ou le max selon le joueur courant. *)
let rec minimax_rec h prof jeu =
    if prof = 0 || jeu.vides = 0 then (h jeu, -1) else begin
       let traiter col =
           if coup_gagnant jeu col jeu.joueur then (1000 * f jeu.joueur, col) 
           else begin
               jouer_coup jeu col;
               let score = minimax_rec h (prof - 1) jeu in
               annuler_coup jeu col;
               fst score, col
           end in
       let lst_score = List.map traiter (coups_possibles jeu) in
       if jeu.joueur = 1 then maxi lst_score else mini lst_score
    end;;

(* Pour avoir la stratégie, il suffit de considérer la deuxième composante
   des valeurs renvoyées par la fonction précédente. *)
let minimax h prof jeu =
    snd (minimax_rec h prof jeu);;

comparer (minimax heuristique 4) strategie_alea;;

(* On commence par écrire une fonction récursive qui calcule un coup (score, coup),
   comme pour l'algorithme min-max. La fonction prend en argument les bornes alpha
   et beta qui sont des références et seront modifiées au cours de la partie. On 
   distingue le cas selon le joueur pour savoir s'il faut utiliser la borne alpha
   ou beta (pour une modification ou un élagage). *)
let rec alphabeta_rec h prof alpha beta jeu =
    if prof = 0 || jeu.vides = 0 then (h jeu, -1)
    else if jeu.joueur = 1 then begin
        let valeur = ref (min_int, -1) in
        let rec traiter = function
            | [] -> ()
            | coup :: q when coup_gagnant jeu coup 1 ->
                valeur := (1000, coup);
                alpha := 1000;
            | coup :: q ->
                jouer_coup jeu coup;
                let score = alphabeta_rec h (prof - 1) alpha beta jeu in
                annuler_coup jeu coup;
                valeur := max !valeur (fst score, coup);
                if fst !valeur <= !beta then begin
                    alpha := max !alpha (fst !valeur);
                    traiter q
                end in
        traiter (coups_possibles jeu);
        !valeur
    end else begin
        let valeur = ref (max_int, -1) in
        let rec traiter = function
            | [] -> ()
            | coup :: q when coup_gagnant jeu coup 2 ->
                valeur := (-1000, coup);
                beta := -1000;
            | coup :: q ->
                jouer_coup jeu coup;
                let score = alphabeta_rec h (prof - 1) alpha beta jeu in
                annuler_coup jeu coup;
                valeur := min !valeur (fst score, coup);
                if fst !valeur >= !alpha then begin
                    beta := min !beta (fst !valeur);
                    traiter q
                end in
        traiter (coups_possibles jeu);
        !valeur
    end;;

(* La fonction principale consiste juste à initialiser les bornes alpha
   et beta et à renvoyer la seconde composante. *)
let alphabeta h prof jeu =
    let alpha = ref min_int and beta = ref max_int in
    snd (alphabeta_rec h prof alpha beta jeu);;

comparer (alphabeta heuristique 2) (minimax heuristique 2);;

(* On détermine le score d'un bloc de 4 cases commençant à la case 
   (lgn, col) et dans la direction (dl, dc), selon le principe
   décrit par l'énoncé. *)
let score_case jeu (lgn, col) (dl, dc) =
    let compte = [|0; 0; 0|] in
    for i = 0 to 3 do
        let x = jeu.grille.(lgn + i * dl).(col + i * dc) in
        compte.(x) <- compte.(x) + 1
    done;
    match compte.(0), compte.(1), compte.(2) with
        | 0, _, 0 -> 1000 | 0, 0, _ -> -1000
        | 1, _, 0 -> 10   | 1, 0, _ -> -10
        | 2, _, 0 -> 3    | 2, 0, _ -> -3
        | 3, _, 0 -> 1    | 3, 0, _ -> -1
        | _ -> 0;;

(* Dès lors, l'heuristique consiste à calculer le score de chaque
   bloc de 4 cases. *)
let heuristique2 jeu =
    let s = ref 0 in
    for lgn = 0 to 5 do
        for col = 0 to 6 do
            if col <= 3 then s := !s + score_case jeu (lgn, col) (0, 1);
            if lgn <= 2 then begin
                s := !s + score_case jeu (lgn, col) (1, 0);
                if col <= 3 then s := !s + score_case jeu (lgn, col) (1, 1);
                if col >= 3 then s := !s + score_case jeu (lgn, col) (1, -1);
            end
        done
    done;
    !s;;

comparer (minimax heuristique 3) (minimax heuristique2 3);;
