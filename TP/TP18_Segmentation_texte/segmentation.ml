
(********************************************************************)
(* Concours Centrale-Supélec                                        *)
(* Sujet 0 - MPI                                                    *)
(* https://www.concours-centrale-supelec.fr                         *)
(* Proposition de corrigé par N. Carré (MPI LLG)                    *)
(* CC BY-NC-SA 3.0                                                  *)
(********************************************************************)

let texte_preface = "préfacedanslaquelleilestétablique,malgréleursnomsenosetenis,leshérosdel'histoirequenousallonsavoirl'honneurderaconteranoslecteursn'ontriendemythologique."

type lexique = (string, int) Hashtbl.t

(* Lexique correspondant à l'exemple proposé dans le sujet *)
let dico_ex : lexique =
  let h = Hashtbl.create 4 in
  let add (m, c) = Hashtbl.add h m c in
  List.iter add [("a", 1); ("ab", 1); ("aba", 2); ("bb", 1)];
  h

(* Vérifie qu'un fichier de lexique est conforme au format demandé *)
let verifie_format nom_fichier =
  let f = open_in nom_fichier in
  begin
    try
      while true do
        match String.split_on_char ' ' (input_line f) with
        | [mot; occ] ->
           assert (String.length mot <= 100);
           assert (int_of_string occ >= 1);
        | _ -> failwith "Erreur de format"
      done
    with End_of_file -> close_in f
  end

let lire_lexique nom_fichier =
    let h = Hashtbl.create 4 in
    let f = open_in nom_fichier in
    begin
        try while true do
            match String.split_on_char ' ' (input_line f) with
                | [mot; occ] -> Hashtbl.add h mot (int_of_string occ)
                | _ -> failwith "Erreur de format"
        done
        with End_of_file -> close_in f
    end;
    h

open Printf

let plus_long_mot h =
    (* Détermine le mot le plus long dans un lexique. *)
    let comparer mot occ mot_max =
        if String.length mot > String.length mot_max then mot
        else mot_max in
    Hashtbl.fold comparer h ""

let nb_mots_uniques h =
    (* Renvoie le nombre de mots n'apparaissant qu'une seule fois
       dans un lexique. *)
    let nb = ref 0 in
    Hashtbl.iter (fun mot occ -> if occ = 1 then incr nb) h;
    !nb

let tests_Q12 () = 
    let nom_fichier = "les_miserables.lex" in
    verifie_format nom_fichier;
    let h = lire_lexique nom_fichier in
    printf "Nombre de mots : %d\n" (Hashtbl.length h);
    printf "Taille du mot le plus long : %d\n" (String.length (plus_long_mot h));
    printf "Mots apparaissant une fois : %d\n" (nb_mots_uniques h)

(* let _ = tests_Q12 () *)

let est_mot h mot = Hashtbl.mem h mot

let score h mot =
    if est_mot h mot then log (float_of_int (Hashtbl.find h mot))
                     else neg_infinity

let existe_segmentation t h =
    (* Teste s'il existe une segmentation de t avec des mots du lexique h. *)
    let n = String.length t in
    let rec existe_rec i j = 
        (* Teste s'il existe une segmentation de t[i:n] dont le premier mot est 
           t[i:i+k], avec j ≤ k. *)
        i = n ||
        (i + j <= n && est_mot h (String.sub t i j) && existe_rec (i + j) 1) ||
        (i + j < n && existe_rec i (j + 1)) in
    existe_rec 0 1

let trouver_segmentation t h =
    (* Renvoie une segmentation de t sous forme de liste de tailles de mots s'il 
       en existe une, ou la liste vide sinon. *)
    let n = String.length t in
    let rec trouver_rec i j = 
        (* Renvoie un couple booléen, segmentation de t[i:n] dont le premier mot 
           est un t[i:i+k] avec j ≤ k, s'il en existe une. Le booléen vaut false 
           s'il n'en existe pas. *)
        if i = n then true, []
        else if i + j > n then false, []
        else if est_mot h (String.sub t i j) then 
                let b, lst = trouver_rec (i + j) 1 in
                if b then true, j :: lst
                else trouver_rec i (j + 1)
            else trouver_rec i (j + 1) in
    snd (trouver_rec 0 1)

let liste_vers_mot t seg =
    (* Prend en argument un texte et une liste de tailles de mots et crée le texte
       avec la segmentation associée, en rajoutant les espaces. *)
    let mot = ref "" in
    let rec completer i = function
        | []     -> !mot
        | [j]    -> mot := !mot ^ String.sub t i j; !mot
        | j :: q -> mot := !mot ^ String.sub t i j ^ " ";
                    completer (i + j) q in
    completer 0 seg       

let tests_Q18 () =
    let dico_3m = lire_lexique "les_trois_mousquetaires.lex" in
    let u = "babbab" and v = "abaaba" and w = texte_preface in
    printf "mot %s : %s\n" u (liste_vers_mot u (trouver_segmentation u dico_ex));
    printf "mot %s : %s\n" v (liste_vers_mot v (trouver_segmentation v dico_ex));
    printf "mot %s : %s\n" w (liste_vers_mot w (trouver_segmentation w dico_3m))

(* let _ = tests_Q18 () *)

let compter_segmentation t h =
    (* Compte le nombre de segmentations de t avec des mots du lexique h. *)
    let n = String.length t in
    let rec compte_rec i = 
        (* Compte le nombre de segmentations de t[i:n]. *)
        if i = n then 1
        else begin
            let nb = ref 0 in
            for j = 1 to n - i do
                if est_mot h (String.sub t i j) then
                    nb := !nb + compte_rec (i + j)
            done;
            !nb
        end in
    compte_rec 0

let tests_Q20 () =
    let dico_mis = lire_lexique "les_miserables.lex" in
    let u = "laquelleestlameilleure" in
    printf "nombre de segmentations : %d\n" (compter_segmentation u dico_mis)

(* let _ = tests_Q20 () *)

let compter_segmentation_memo t h =
    (* Même fonction que précédemment, mais mémoïsée. *)
    let n = String.length t in
    let tab = Array.make (n + 1) (-1) in
    tab.(n) <- 1;
    let rec compte_memo i =
        if tab.(i) = -1 then begin
            tab.(i) <- 0;
            for j = 1 to n - i do
                if est_mot h (String.sub t i j) then
                    tab.(i) <- tab.(i) + compte_memo (i + j)
            done;
        end;
        tab.(i) in
    compte_memo 0

let tests_Q23 () = 
    let dico_mis = lire_lexique "les_miserables.lex" in
    let u = "laquelleestlameilleure" and v = texte_preface in
    printf "nombre de segmentations : %d\n" (compter_segmentation_memo u dico_mis);
    printf "nombre de segmentations : %d\n" (compter_segmentation_memo v dico_mis)

(* let _ = tests_Q23 () *)

let segmentation_score_max t h =
    (* Renvoie la segmentation de score maximal, ainsi que son score. On construit
       un tableau tab telle que tab.(i) contient un couple (score, seg) où 
       score est le score maximal d'une segmentation de t[i:n], et seg la segmentation 
       associée. *)
    let n = String.length t in
    let tab = Array.make (n + 1) (-1., []) in
    tab.(n) <- (0., []);
    let rec seg_score_rec i =
        if fst tab.(i) < 0. then begin
            tab.(i) <- 0., [];
            for j = 1 to n - i do
                let u = String.sub t i j in
                let sc, seg = seg_score_rec (i + j) in
                let nouv_sc = sc +. score h u in
                if nouv_sc > fst tab.(i) then
                    tab.(i) <- nouv_sc, j :: seg
            done
        end;
        tab.(i) in
    seg_score_rec 0

let tests_Q25 () =
    let dico_mis = lire_lexique "les_miserables.lex" in
    let u = "laquelleestlameilleure" and v = texte_preface in
    let c1 = segmentation_score_max u dico_mis and
        c2 = segmentation_score_max v dico_mis in
    printf "Segmentation de %s : %s\n" u (liste_vers_mot u (snd c1));
    printf "Score de %s : %f\n" v (fst c2)

(* let _ = tests_Q25 () *)

let segmentation_score_max_bis t h =
    (* Comme la fonction précédente, avec le score modifié. *)
    let n = String.length t in
    let tab = Array.make (n + 1) (-1., []) in
    tab.(n) <- (0., []);
    let rec seg_score_rec i =
        if fst tab.(i) < 0. then begin
            tab.(i) <- 0., [];
            for j = 1 to n - i do
                let u = String.sub t i j in
                let sc, seg = seg_score_rec (i + j) in
                let nouv_sc = sc +. score h u +. float_of_int (j * j) in
                if nouv_sc > fst tab.(i) then
                    tab.(i) <- nouv_sc, j :: seg
            done
        end;
        tab.(i) in
    seg_score_rec 0

let tests_Q27 () =
    let dico_mis = lire_lexique "les_miserables.lex" and
        dico_3m = lire_lexique "les_trois_mousquetaires.lex" and
        dico_ch = lire_lexique "la_comedie_humaine.lex" in 
    let u = texte_preface in
    let c1 = segmentation_score_max_bis u dico_mis  and
        c2 = segmentation_score_max_bis u dico_3m and
        c3 = segmentation_score_max_bis u dico_ch in 
    let afficher c =
        printf "Score : %f et segmentation : %s\n" (fst c) (liste_vers_mot u (snd c)) in
    afficher c1; afficher c2; afficher c3

(* let _ = tests_Q27 () *)

let segmentation_score_max_big t h hbig =
    (* Comme la fonction précédente, en prenant en compte les bigrammes. *)
    let n = String.length t in
    let tab = Array.make (n + 1) (-1., []) in
    tab.(n) <- (0., []);
    let rec seg_score_rec i =
        if fst tab.(i) < 0. then begin
            tab.(i) <- 0., [];
            for j = 1 to n - i do
                let u = String.sub t i j in
                let sc, seg = seg_score_rec (i + j) in
                let sc_big = if i + j = n || seg = [] then 0.
                             else score hbig (u ^ " " ^ String.sub t (i + j) (List.hd seg)) in
                let nouv_sc = sc +. score h u +. float_of_int (j * j) +. sc_big in
                if nouv_sc > fst tab.(i) then
                    tab.(i) <- nouv_sc, j :: seg
            done
        end;
        tab.(i) in
    seg_score_rec 0

let lire_big nom_fichier =
    let h = Hashtbl.create 4 in
    let f = open_in nom_fichier in
    begin
        try while true do
            match String.split_on_char ' ' (input_line f) with
                | [mot1; mot2; occ] -> Hashtbl.add h (mot1 ^ " " ^ mot2) (int_of_string occ)
                | _ -> failwith "Erreur de format"
        done
        with End_of_file -> close_in f
    end;
    h

let tests_Q29 () =
    let dico_mis = lire_lexique "les_miserables.lex" and
        dico_3m = lire_lexique "les_trois_mousquetaires.lex" and
        dico_ch = lire_lexique "la_comedie_humaine.lex" and
        dico_big_mis = lire_big "les_miserables.big" and
        dico_big_3m = lire_big "les_trois_mousquetaires.big" and
        dico_big_ch = lire_big "la_comedie_humaine.big" in
    let u = texte_preface in
    let c1 = segmentation_score_max_big u dico_mis dico_big_mis and
        c2 = segmentation_score_max_big u dico_3m dico_big_3m and
        c3 = segmentation_score_max_big u dico_ch dico_big_ch in 
    let afficher c =
        printf "Score : %f et segmentation : %s\n" (fst c) (liste_vers_mot u (snd c)) in
    afficher c1; afficher c2; afficher c3

(* let _ = tests_Q29 () *)