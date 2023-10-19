(* On lance un appel récursif sur la queue et on ajoute
   la tête à la bonne liste en fonction de sa comparaison
   avec le pivot. *)
let rec partition piv = function
    | []     -> [], []
    | x :: q -> let l1, l2 = partition piv q in
                if x <= piv then x :: l1, l2
                            else l1, x :: l2;;

(* Si la liste est de taille 0 ou 1, elle est déjà triée,
   sinon on partition la queue selon la tête, on trie les
   deux moitiés récursivement, et on recolle le tout. *)
let rec tri_rapide lst = match lst with
    | [] | [_] -> lst
    | piv :: q -> let l1, l2 = partition piv q in
                  tri_rapide l1 @ piv :: tri_rapide l2;;

(* Quelques tests : on crée une liste aléatoire, on
   l'affiche, ainsi que sa version triée. On teste
   également sur des listes de taille 0 et 1. *)
let liste_alea n = List.init n (fun _ -> Random.int 20);;

let rec afficher_liste = function
    | []     -> print_newline ()
    | x :: q -> Printf.printf "%d " x; afficher_liste q;;

let _ =
    print_endline "Test du tri rapide.";
    Random.self_init ();
    let lst = liste_alea 20 in
    afficher_liste lst;
    afficher_liste (tri_rapide lst);
    afficher_liste (tri_rapide []);
    afficher_liste (tri_rapide [10]);;

(* On commence par ouvrir le canal et lire la taille du graphe. 
   On crée le graphe, puis on lit chaque ligne du fichier. On 
   crée l'arête correspondante à chaque fois. On gère la levée
   d'exception avec try … with. *)

let construire_graphe nom_fichier =
    let canal = open_in nom_fichier in
    let ios = int_of_string in
    let n = ios (input_line canal) in
    let g = Array.make n [] in
    try while true do
        let s, t, p = match String.split_on_char ' ' (input_line canal) with
           | [s'; t'; p'] -> ios s', ios t', ios p'
           | _            -> failwith "Mauvais format" in
        g.(s) <- (t, p) :: g.(s);
        g.(t) <- (s, p) :: g.(t)
    done;
    g
    with End_of_file -> close_in canal; g;;

(* On construit les différents graphes et on crée une liste les contenant. *)

let g_10_ad = construire_graphe "g_10_ad.txt";;
let g_10_sd = construire_graphe "g_10_sd.txt";;
let g_1000_ad = construire_graphe "g_1000_ad.txt";;
let g_1000_sd = construire_graphe "g_1000_sd.txt";;
let g_50000_ad = construire_graphe "g_50000_ad.txt";;
let g_50000_sd = construire_graphe "g_50000_sd.txt";;

let graphes = [g_10_ad; g_10_sd; g_1000_ad; g_1000_sd; g_50000_ad; g_50000_sd];;

(* On utilise une référence et on parcourt toutes les listes d'adjacence. On
   n'ajoute le poids d'une arête que si s < t, pour éviter de les compter 
   deux fois (ce qu'on aurait pu faire en divisant par deux à la fin). *)

let poids_graphe g =
    let n = Array.length g in
    let poids = ref 0 in
    for s = 0 to n - 1 do
        let aux (t, p) =
            if s < t then poids := !poids + p in
        List.iter aux g.(s)
    done;
    !poids;;

let _ =
    let aux i g = 
        Printf.printf "Le poids du graphe d'indice %d est %d\n" i (poids_graphe g) in
    List.iteri aux graphes;;

(* Une partition de taille n est simplement un tableau de -1, car 
   les singletons sont de rang nul. *)
let creer_partition n = Array.make n (-1);;

(* Comme la fonction du cours (mais en OCaml). *)
let rec trouver partition x =
    if partition.(x) < 0 then x
    else begin
       let rx = trouver partition partition.(x) in
       partition.(x) <- rx;
       rx
    end;;

(* Similaire à la fonction du cours, sauf qu'on traite le cas
   d'égalité différemment. On garde bien en mémoire que si 
   partition.(rx) < partition.(ry), alors le rang de rx est 
   SUPÉRIEUR à celui de ry (à cause des valeurs négatives). *)
let unir partition x y =
    let rx = trouver partition x and ry = trouver partition y in
    if rx <> ry then begin
        if partition.(rx) < partition.(ry) then
            partition.(ry) <- rx
        else if partition.(rx) > partition.(ry) then
            partition.(rx) <- ry
        else begin
            partition.(rx) <- partition.(rx) - 1;
            partition.(ry) <- rx
        end
    end;;

(* Similaire à la fonction poids_graphe. On trie la liste une fois
   qu'elle est créée (on utilise l'ordre lexicographique comme ordre
   naturel, c'est pour ça qu'on met les poids en premier). *)
let aretes g =
    let n = Array.length g in
    let lst_aretes = ref [] in
    for s = 0 to n - 1 do
        let aux (t, p) =
            if s < t then lst_aretes := (p, s, t) :: !lst_aretes in
        List.iter aux g.(s)
    done;
    tri_rapide !lst_aretes;;

(* Un petit test. *)
let _ =
    let lst_aretes = aretes g_10_sd in
    let p, s, t = List.hd lst_aretes in
    Printf.printf "L'arête de poids minimal de g_10_sd est (%d, %d), de poids %d\n" s t p;
    let p', s', t' = List.hd (List.rev lst_aretes) in
    Printf.printf "L'arête de poids maximal de g_10_sd est (%d, %d), de poids %d\n" s' t' p';;

(* Après avoir créé un graphe pour l'arbre couvrant et la partition, on
   crée une fonction auxiliaire qui permet de traiter individuellement
   une arête (la rajouter à l'arbre et fusionner les deux classes ou ne
   rien faire). On l'applique à chaque arête dans l'ordre donné par la
   fonction aretes. *)
let kruskal g =
    let n = Array.length g in
    let acm = Array.make n [] in
    let partition = creer_partition n in
    let traiter_arete (p, s, t) =
        let rs = trouver partition s and rt = trouver partition t in
        if rs <> rt then begin
           acm.(s) <- (t, p) :: acm.(s);
           acm.(t) <- (s, p) :: acm.(t);
           unir partition s t
        end in
    List.iter traiter_arete (aretes g);
    acm;;

(* On calcule les poids des ACM. *)
let _ =
    let aux i g = 
        Printf.printf "Le poids de l'arbre couvrant minimal du graphe d'indice %d est %d\n" 
                      i (poids_graphe (kruskal g)) in
    List.iteri aux graphes;;