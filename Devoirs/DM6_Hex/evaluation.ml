type hex = {plateau : int array array;
            mutable joueur : int}

(* Initialise une partie d'othello *)
let init_hex () =
    let plateau = Array.make_matrix 11 11 0 in
    {plateau; joueur = 1}

(* Détermine si un coup est possible, c'est-à-dire si la case est libre *)
let coup_possible h (i, j) = h.plateau.(i).(j) = 0

(* Renvoie la liste des coups possibles *)
let coups_possibles h =
    let coups = ref [] in
    for i = 0 to 10 do
        for j = 0 to 10 do
            if coup_possible h (i, j) then coups := (i, j) :: !coups
        done
    done;
    !coups

(* Joue un coup sur le plateau *)
let jouer_coup h (i, j) =
    assert (coup_possible h (i, j));
    h.plateau.(i).(j) <- h.joueur;
    h.joueur <- 3 - h.joueur

(* Une stratégie qui renvoie un coup aléatoire du joueur *)
let strategie_alea h =
    Random.self_init ();
    let coups = Array.of_list (coups_possibles h) in
    let n = Array.length coups in
    if n = 0 then (-1, -1)
    else coups.(Random.int n)

(* Fonction d'affichage du plateau *)
let afficher h =
    let open Printf in
    let symb = [|"\x1b[34m\x1b[4m.\x1b[0m"; "\x1b[31m\x1b[4m●\x1b[0m"; "\x1b[33m\x1b[4m●\x1b[0m"|] in
    let symb2 = [|"\x1b[34m.\x1b[0m"; "\x1b[31m●\x1b[0m"; "\x1b[33m●\x1b[0m"|] in
    print_string "  \x1b[34m             0 1 2 3 4 5 6 7 8 9 10\x1b[0m\n";
    print_string "  \x1b[33m            _ _ _ _ _ _ _ _ _ _ _\x1b[0m\n";
    for i = 0 to 10 do
        for j = 0 to 9 - i do
            print_string " "
        done;
        printf "\x1b[34m\x1b[1m%d%s\x1b[0m \x1b[31m|\x1b[0m" i
            (if i < 10 then " " else "");
        for j = 0 to 9 do
            if i < 10 then
                printf "%s\x1b[34m|\x1b[0m" symb.(h.plateau.(i).(j))
            else
                printf "%s\x1b[34m|\x1b[0m" symb2.(h.plateau.(i).(j))
        done;
        if i < 10 then
            printf "%s\x1b[31m|\x1b[0m" symb.(h.plateau.(i).(10))
        else 
            printf "%s\x1b[31m|\x1b[0m" symb2.(h.plateau.(i).(10));
        print_newline ();
    done;
    print_string "   \x1b[33m ‾ ‾ ‾ ‾ ‾ ‾ ‾ ‾ ‾ ‾ ‾\x1b[0m\n"

exception Victoire of int

(* Renvoie les indices des voisins d'une case du plateau *)
let voisins (i, j) =
    let test (k, l) = 
        k >= 0 && k < 11 && l >= 0 && l < 11
    in
    List.filter test [(i-1, j); (i+1, j); (i, j-1); (i, j+1); (i+1,j+1); (i-1, j-1)]

(* Fonction récursive de parcours en profondeur pour tester l'existence de chemin gagnant *)
let rec dfs h acces (i, j) joueur =
    if acces.(i).(j) = 0 then begin
        acces.(i).(j) <- joueur;
        if joueur = 1 && j = 10 then raise (Victoire 1);
        if joueur = 2 && i = 10 then raise (Victoire 2);
        let traiter (k, l) =
            if h.plateau.(k).(l) = joueur then dfs h acces (k, l) joueur
        in
        List.iter traiter (voisins (i, j))
    end

(* Détermine si un joueur est gagnant ou pas encore *)
let gagnant h =
    let acces = Array.make_matrix 11 11 0 in
    try
        for i = 0 to 10 do
            if h.plateau.(i).(0) = 1 then dfs h acces (i, 0) 1;
            if h.plateau.(0).(i) = 2 then dfs h acces (0, i) 2
        done;
        0
    with
        Victoire joueur -> joueur

(* Calcul du score *)
let score h =
    let points = [|0; 0|] in
    let g = gagnant h in
    assert (g > 0);
    points.(g - 1) <- List.length (coups_possibles h);
    points

(* Fonction pour recopier une matrice dans une autre
   (parce que je ne fais pas confiance aux élèves
   pour ne pas modifier le plateau) *)
let recopie mat1 mat2 =
    for i = 0 to Array.length mat1 - 1 do
        for j = 0 to Array.length mat1.(0) - 1 do
            mat2.(i).(j) <- mat1.(i).(j)
        done
    done

(* Les lignes suivantes me permettent d'importer les fichiers
   et de mettre les bonnes stratégies dans un tableau, ainsi
   que d'avoir les suffixes / noms pour chaque joueur. *)

(*
#mod_use "hex_asst.ml";;
#mod_use "hex_auza.ml";;
#mod_use "hex_baza.ml";;
#mod_use "hex_beua.ml";;
#mod_use "hex_burg.ml";;
#mod_use "hex_chic.ml";;
#mod_use "hex_dubm.ml";;
#mod_use "hex_fiso.ml";;
#mod_use "hex_guyc.ml";;
#mod_use "hex_hirl.ml";;
#mod_use "hex_jonm.ml";;
#mod_use "hex_lann.ml";;
#mod_use "hex_marv.ml";;
#mod_use "hex_mirg.ml";;
#mod_use "hex_mona.ml";;
#mod_use "hex_ngua.ml";;
#mod_use "hex_oujm.ml";;
#mod_use "hex_peyl.ml";;
#mod_use "hex_reml.ml";;
#mod_use "hex_sabe.ml";;
#mod_use "hex_salf.ml";;*)

let strategies = [|
    strategie_alea;
    (*Hex_asst.strategie;
    Hex_auza.strategie;
    Hex_baza.strategie;
    Hex_beua.strategie;
    Hex_burg.strategie;
    Hex_chic.strategie;
    Hex_dubm.strategie;
    Hex_fiso.strategie;
    Hex_guyc.strategie;
    Hex_hirl.strategie;
    Hex_jonm.strategie;
    Hex_lann.strategie;
    Hex_marv.strategie;
    Hex_mirg.strategie;
    Hex_mona.strategie;
    Hex_ngua.strategie;
    Hex_oujm.strategie;
    Hex_peyl.strategie;
    Hex_reml.strategie;
    Hex_sabe.strategie;
    Hex_salf.strategie;*)
|]

let prenoms = [|
    "Aléatoire";
    (*"Théodore";
    "Adam";
    "Alberie";
    "Alexandre B.";
    "Gabriel B.";
    "Cameron";
    "Matthieu";
    "Oscar";
    "Clovis";
    "Léandre";
    "Matthias";
    "Nathan";
    "Valentin";
    "Gaspard";
    "Alexandre M.";
    "Anh Duc";
    "Mouad";
    "Laurette";
    "Léon";
    "Esteban";
    "Florian";*)
|]

(* Simule une partie entre deux stratégies *)
let partie a b =
    let strats = [|strategies.(a); strategies.(b)|] in
    let h = init_hex () and copie = Array.make_matrix 11 11 0 in
    let fini = ref false and temps = [|0.; 0.|] in
    let malus = ref 0 in
    while not !fini do
        let coups = coups_possibles h in
        if coups = [] then begin
            h.joueur <- 3 - h.joueur;
            if coups_possibles h = [] then fini := true            
        end 
        else begin
            let debut = Sys.time () in
            recopie h.plateau copie;
            let j = h.joueur in
            let coup = 
                try strats.(j - 1) h 
                with _ -> (-1, -1)
            in
            temps.(j - 1) <- temps.(j - 1) +. Sys.time () -. debut;
            recopie copie h.plateau;            
            if temps.(j - 1) > 120. then begin
                Printf.printf "Temps trop long pour le joueur %s !\n" 
                    prenoms.(if j = 1 then a else b);
                fini := true;
                malus := 1 - j + j mod 2;
            end
            else if coup = (-1, -1) then begin
                Printf.printf "Erreur de calcul de stratégie pour %s !\n" 
                        prenoms.(if j = 1 then a else b);
                fini := true;
                malus := (1 - j + j mod 2) * 3;
            end
            else if coup_possible h coup then begin
                jouer_coup h coup;
                if gagnant h > 0 then fini := true
            end
            else begin
                Printf.printf "Coup impossible par %s en tant que joueur %d !\n" 
                    prenoms.(if j = 1 then a else b) j;
                Printf.printf "Le coup impossible est ligne %d, colonne %d\n" 
                    (fst coup) (snd coup);
                fini := true;
                malus := (1 - j + j mod 2) * 2;
            end
        end
    done;
    let scr = [|0; 0|] in
    if !malus < 0 then begin
        scr.(0) <- 121 - !malus;
    end
    else if !malus > 0 then begin
        scr.(1) <- 121 + !malus
    end
    else begin
        let s = score h in
        scr.(0) <- s.(0);
        scr.(1) <- s.(1)
    end;
    h, scr

(* Effectue un duel entre deux stratégies. Le booléen aff permet 
   d'indiquer si on souhaite un affichage ou non. *)
let duel a b aff =
    let p1, points1 = partie a b and p2, points2 = partie b a in
    if aff then begin
        afficher p1;
        afficher p2;
        Printf.printf "La première partie s'achève sur un score %d/%d, la deuxième partie sur un score %d/%d\n" 
        points1.(0) points1.(1) points2.(0) points2.(1);
    end;
    let g0 = points1.(0) + points2.(1) and g1 = points1.(1) + points2.(0) in
    if g0 > g1 then a else if g1 > g0 then b else -1

(* Permet de faire un duel avec les affichages des joueurs et
   du gagnant. *)
let affrontement a b =
    Printf.printf "Match entre les joueurs %s et %s !\n" prenoms.(a) prenoms.(b);
    Printf.printf "%!";
    let x = duel a b true in
    if x = -1 then Printf.printf "C'est un match nul ! La victoire est à départager au Shifumi.\n"
    else Printf.printf "C'est %s (n°%d) qui remporte la partie !\n" prenoms.(x) x

(* Lance les qualifications : détermine pour chaque joueur s'il est
   capable de faire une partie sans erreur et de gagner contre une
   stratégie aléatoire. *)
let qualifications debut fin =
    let n = Array.length prenoms in
    for b = debut to min fin (n - 1) do
        Printf.printf "Joueur %s :\n" prenoms.(b);
        try 
            let d = duel 0 b false in
            if d = 0 then 
                print_endline "\t éliminé par le joueur aléatoire.\n"
            else print_endline "\t passe les qualifications.\n"
        with 
            | Invalid_argument s ->                 
                Printf.printf "\t éliminé pour erreur %s.\n\n" s
            | _ -> print_endline "\t éliminé par une levée d'exception.\n"
    done