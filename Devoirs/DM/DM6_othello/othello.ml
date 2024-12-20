type othello = {plateau : int array;
                mutable joueur : int}

(* Initialise une partie d'othello *)
let init_othello () =
    let plateau = Array.make 64 0 in
    plateau.(27) <- 2;
    plateau.(28) <- 1;
    plateau.(35) <- 1;
    plateau.(36) <- 2;
    {plateau; joueur = 1}

(* Teste si un couple (ligne, colonne) est dans les bornes du plateau *)
let valide li co =
    0 <= li && li < 8 && 0 <= co && co < 8

(* Tableau des directions : différentiel d'indices unidimensionnels *)
let direction = [|-9; -8; -7; -1; 1; 7; 8; 9|]

(* Détermine s'il y a des pions à retourner depuis une position donnée,
   dans une direction donnée *)
let coup_possible_dir oth i dir =
    let li = ref (i / 8) and co = ref (i mod 8) and
        dl = dir / 7 and dc = ((dir + 9) mod 8) - 1 in
    let au_moins_un = ref false in
    let pos = ref i in
    li := !li + dl; co := !co + dc; pos := !pos + dir;
    while valide !li !co && oth.plateau.(!pos) = 3 - oth.joueur do
        li := !li + dl; co := !co + dc; pos := !pos + dir;
        au_moins_un := true
    done;
    !au_moins_un && valide !li !co && oth.plateau.(!pos) = oth.joueur

(* Détermine si un coup est possible à une position donnée *)
let coup_possible oth i =
    if not (0 <= i && i < 64 && oth.plateau.(i) = 0) then false
    else begin
        let possible = ref false in
        for k = 0 to 7 do
            possible := !possible || coup_possible_dir oth i direction.(k)
        done;
        !possible
    end

(* Renvoie la liste des coups possibles *)
let coups_possibles oth =
    List.filter (coup_possible oth) (List.init 64 Fun.id)

(* Retourne tous les pions dans une direction donnée pour un coup valide *)
let retourner_pions_dir oth i dir =
    let li = ref (i / 8) and co = ref (i mod 8) and
        dl = dir / 7 and dc = ((dir + 9) mod 8) - 1 in
    let pos = ref i in
    li := !li + dl; co := !co + dc; pos := !pos + dir;
    while valide !li !co && oth.plateau.(!pos) = 3 - oth.joueur do
        oth.plateau.(!pos) <- oth.joueur;
        li := !li + dl; co := !co + dc; pos := !pos + dir;
    done

(* Joue un coup sur le plateau d'othello *)
let jouer_coup oth i =
    for k = 0 to 7 do
        if coup_possible_dir oth i direction.(k) then
            retourner_pions_dir oth i direction.(k)
    done;
    oth.plateau.(i) <- oth.joueur;
    oth.joueur <- 3 - oth.joueur

(* Une stratégie qui renvoie un coup aléatoire du joueur *)
let strategie_alea oth =
    Random.self_init ();
    let coups = Array.of_list (coups_possibles oth) in
    let n = Array.length coups in
    if n = 0 then -1
    else coups.(Random.int n)

(* Fonction d'affichage du plateau *)
let afficher oth =
    let open Printf in
    let symb = [|"\x1b[34m\x1b[4m \x1b[0m"; "\x1b[31m\x1b[4m●\x1b[0m"; "\x1b[33m\x1b[4m●\x1b[0m"|] in
    print_string "  \x1b[34m _ _ _ _ _ _ _ _\x1b[0m\n";
    for i = 0 to 7 do
        printf "\x1b[34m\x1b[1m%d\x1b[0m \x1b[34m|\x1b[0m" i;
        for j = 0 to 7 do
            printf "%s\x1b[34m|\x1b[0m" symb.(oth.plateau.(8 * i + j))
        done;
        print_newline ();
    done;
    print_string "  \x1b[34m\x1b[1m 0 1 2 3 4 5 6 7\x1b[0m\n"

(* Calcul du score *)
let score oth =
    let points = [|0; 0|] in
    for i = 0 to 63 do
        let k = oth.plateau.(i) - 1 in
        if k >= 0 then
            points.(k) <- 1 + points.(k)
    done;
    if points.(0) = 0 then points.(1) <- 64;
    if points.(1) = 0 then points.(0) <- 64;
    points

(* Fonction pour recopier un tableau dans un autre
   (parce que je ne fais pas confiance aux élèves
   pour ne pas modifier le plateau) *)
let recopie tab1 tab2 =
    for i = 0 to Array.length tab1 - 1 do
        tab2.(i) <- tab1.(i)
    done

(* Les lignes suivantes me permettent d'importer les fichiers
   et de mettre les bonnes stratégies dans un tableau, ainsi
   que d'avoir les suffixes / noms pour chaque joueur.
#mod_use "othello_akus.ml";;
#mod_use "othello_bacj.ml";;
#mod_use "othello_beaj.ml";;
#mod_use "othello_blaa.ml";;
#mod_use "othello_choa.ml";;
#mod_use "othello_cosm.ml";;
#mod_use "othello_ench.ml";;
#mod_use "othello_hola.ml";;
#mod_use "othello_ibri.ml";;
#mod_use "othello_isri.ml";;
#mod_use "othello_marj.ml";;
#mod_use "othello_molb.ml";;
#mod_use "othello_nguv.ml";;
#mod_use "othello_romv.ml";;
#mod_use "othello_tres.ml";;
#mod_use "othello_vane.ml";;

let strategies = [|
    strategie_alea;
    Othello_akus.strategie;
    Othello_bacj.strategie;
    Othello_beaj.strategie;
    Othello_blaa.strategie;
    Othello_choa.strategie;
    Othello_cosm.strategie;
    Othello_ench.strategie;
    Othello_hola.strategie;
    Othello_ibri.strategie;
    Othello_isri.strategie;
    Othello_marj.strategie;
    Othello_molb.strategie;
    Othello_nguv.strategie;
    Othello_romv.strategie;
    Othello_tres.strategie;
    Othello_vane.strategie;
|]

let suffixes = [|
    "alea";
    "akus";
    "bacj";
    "beaj";
    "blaa";
    "choa";
    "cosm";
    "ench";
    "hola";
    "ibri";
    "isri";
    "marj";
    "molb";
    "nguv";
    "romv";
    "tres";
    "vane";
|];;

let prenoms = [|
    "Aléatoire";
    "Stepan";
    "Julie";
    "Jules";
    "Adèle";
    "Axel";
    "Mimmo";
    "Hermin";
    "Antoine";
    "Idrisse";
    "Itaï";
    "Joachim";
    "Baptiste";
    "Van-Kevin";
    "Valentin";
    "Simon";
    "Eliott"
|]
*)

(* Simule une partie entre deux stratégies *)
let partie a b =
    let strats = [|strategies.(a); strategies.(b)|] in
    let oth = init_othello () in
    let fini = ref false and temps = [|0.; 0.|] in
    let malus = ref 0 in
    while not !fini do
        let coups = coups_possibles oth in
        if coups = [] then begin
            oth.joueur <- 3 - oth.joueur;
            if coups_possibles oth = [] then fini := true            
        end 
        else begin
            let debut = Sys.time () in
            let plat = Array.copy oth.plateau in
            let j = oth.joueur in
            let coup = 
                try strats.(j - 1) oth 
                with _ -> -1
            in
            temps.(j - 1) <- temps.(j - 1) +. Sys.time () -. debut;
            recopie plat oth.plateau;            
            if temps.(j - 1) > 120. then begin
                Printf.printf "Temps trop long pour le joueur %s !\n" 
                    suffixes.(if j = 1 then a else b);
                fini := true;
                malus := 1 - j + j mod 2;
            end
            else if coup = -1 then begin
                Printf.printf "Erreur de calcul de stratégie pour %s !\n" 
                        suffixes.(if j = 1 then a else b);
                fini := true;
                malus := (1 - j + j mod 2) * 3;
            end
            else if coup_possible oth coup then 
                jouer_coup oth coup
            else begin
                Printf.printf "Coup impossible par %s en tant que joueur %d !\n" 
                    suffixes.(if j = 1 then a else b) j;
                Printf.printf "Le coup impossible est ligne %d, colonne %d\n" 
                    (coup / 8) (coup mod 8);
                fini := true;
                malus := (1 - j + j mod 2) * 2;
            end
        end
    done;
    let scr = score oth in
    if !malus < 0 then begin
        scr.(0) <- 64 - !malus;
        scr.(1) <- 0;
    end
    else if !malus > 0 then begin
        scr.(0) <- 0;
        scr.(1) <- 64 + !malus
    end;
    oth, scr

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
    else Printf.printf "C'est %s qui remporte la partie !\n" prenoms.(x)

(* Lance les qualifications : détermine pour chaque joueur s'il est
   capable de faire une partie sans erreur et de gagner contre une
   stratégie aléatoire. *)
let qualifications debut fin =
    let n = Array.length suffixes in
    for b = debut to min fin (n - 1) do
        Printf.printf "Joueur %s :\n" suffixes.(b);
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