type awale = int array array * int;;

let init_awale () = (Array.make_matrix 2 6 4, 0);;

let suivant = function
    | (0, 0) -> (1, 0)
    | (1, 5) -> (0, 5)
    | (0, i) -> (0, i - 1)
    | (_, i) -> (1, i + 1);;

let precedent = function
    | (1, 0) -> (0, 0)
    | (0, 5) -> (1, 5)
    | (0, i) -> (0, i + 1)
    | (_, i) -> (1, i - 1);;

let semer (plateau, j) i =
    assert (plateau.(j).(i) > 0);
    let coord = ref (j, i) in
    while plateau.(j).(i) > 0 do
        coord := suivant !coord;
        if !coord <> (j, i) then begin
            let (j', i') = !coord in
            plateau.(j').(i') <- plateau.(j').(i') + 1;
            plateau.(j).(i) <- plateau.(j).(i) - 1
        end
    done;
    !coord;;

let a_recolter plateau (j, i) =
    plateau.(j).(i) = 2 || plateau.(j).(i) = 3;;

let recolter (plateau, j) coord =
    let coord = ref coord and graines = ref 0 in
    while fst !coord <> j && a_recolter plateau !coord do
        let j', i' = !coord in
        graines := !graines + plateau.(j').(i');
        plateau.(j').(i') <- 0;
        coord := precedent !coord
    done;
    !graines;;

let famine (plateau, j) =
    Array.for_all ((=) 0) plateau.(j);;

let deepcopy mat =
    let copie = Array.copy mat in
    for i = 0 to Array.length mat - 1 do
        copie.(i) <- Array.copy mat.(i)
    done;
    copie;;

let coup_possible (plateau, j) i =
    plateau.(j).(i) > 0 && begin 
        let copie = deepcopy plateau in
        let coord = semer (copie, j) i in
        ignore (recolter (copie, j) coord);
        not (famine (copie, 1 - j))
    end;;

let filter f tab =
    Array.of_list (List.filter f (Array.to_list tab));;

let coups_possibles jeu =
    filter (coup_possible jeu) (Array.init 6 Fun.id);;

let strategie_alea jeu =
    Random.self_init ();
    let coups = coups_possibles jeu in
    let n = Array.length coups in
    if n = 0 then -1
    else coups.(Random.int n);;

let afficher plateau =    
    for j = 0 to 1 do
        print_string " __ __ __ __ __ __\n";
        print_string "|";
        for i = 0 to 5 do
            Printf.printf "%s%d|" (if plateau.(j).(i) < 10 then " " else "") plateau.(j).(i)
        done;
        print_newline ();
    done;;

let cle (plateau, j) =
    Array.to_list (Array.map Array.to_list plateau), j;;

let somme tab = Array.fold_left (+) 0 tab;;

#mod_use "awale_natc.ml";;

let strategies = [|
    strategie_alea;
	Awale_natc.strategie;

|];;

let suffixes = [|
    "alea";
    "natc";
|];;

let partie a b =
    let strats = [|strategies.(a); strategies.(b)|] in
    let jeu = ref (init_awale ()) in
    let deja_vu = Hashtbl.create 1 in
    let graines = [|0; 0|] and fini = ref false and
        temps = [|0.; 0.|] in
    while not !fini do
        let (plateau, j) = !jeu in
        let cle_jeu = cle !jeu in
        if Hashtbl.mem deja_vu cle_jeu then begin
            fini := true;
            for j = 0 to 1 do
                graines.(j) <- graines.(j) + somme plateau.(j)
            done
        end else begin
            Hashtbl.add deja_vu cle_jeu 1;
            let coups = coups_possibles !jeu in
            if coups = [||] then begin
                fini := true;
                graines.(j) <- graines.(j) + somme (Array.map somme plateau);
            end else begin
                let debut = Sys.time () in
                let coup = strats.(j) (deepcopy plateau, j) in
                temps.(j) <- temps.(j) +. Sys.time () -. debut;
                if temps.(j) > 120. then begin
                    Printf.printf "Temps trop long pour le joueur %s !\n" 
                        suffixes.(if j = 0 then a else b);
                    fini := true;
                    graines.(j) <- 0;
                    graines.(1 - j) <- 49
                end;
                if coup_possible !jeu coup then begin
                    let coord = semer !jeu coup in
                    graines.(j) <- graines.(j) + recolter !jeu coord;
                    jeu := (plateau, 1 - j)
                end else begin
                    Printf.printf "Coup impossible par %s en tant que joueur %d !\n" 
                        suffixes.(if j = 0 then a else b) j;
                    afficher plateau;
                    Printf.printf "Le coup impossible est %d\n" coup;
                    fini := true;
                    graines.(j) <- 0;
                    graines.(1 - j) <- 50
                end
            end
        end
    done;
    graines;;

let duel a b aff =
    let p1 = partie a b and p2 = partie b a in
    if aff then Printf.printf "La première partie s'achève sur un score %d/%d, la deuxième partie sur un score %d/%d\n" p1.(0) p1.(1) p2.(0) p2.(1);
    let g0 = p1.(0) + p2.(1) and g1 = p1.(1) + p2.(0) in
    if g0 > g1 then a else if g1 > g0 then b else -1;;

let qualifications debut =
    let n = Array.length suffixes in
    for b = debut to n - 1 do
        Printf.printf "Joueur %s :\n" suffixes.(b);
        try 
            let d = duel 0 b false in
            if d = 0 then 
                print_endline "\t éliminé par le joueur aléatoire.\n"
            else print_endline "\t passe les préliminaires.\n"
        with 
            | Invalid_argument s ->                 
                Printf.printf "\t éliminé pour erreur %s.\n\n" s
            | _ -> print_endline "\t éliminé par une levée d'exception.\n"
    done;;

let melange tab =
    Random.self_init ();
    let n = Array.length tab in
    for i = n downto 2 do
        let j = Random.int i in
        let tmp = tab.(j) in
        tab.(j) <- tab.(i - 1);
        tab.(i - 1) <- tmp;
    done;
    tab;;