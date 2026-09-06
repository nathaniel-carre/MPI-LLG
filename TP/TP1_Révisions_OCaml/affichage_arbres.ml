type arbre_bin = Vide | N of int * arbre_bin * arbre_bin
type arbre = Noeud of int * arbre list

(*****************ARBRES BINAIRES*******************)
(* type arbre_bin = Vide | N of int * arbre_bin * arbre_bin;; *)

let racine_bin = function
    | Vide       -> failwith "Vide"
    | N(x, _, _) -> string_of_int x

let enfants_bin = function
    | Vide       -> failwith "Vide"
    | N(_, g, d) -> g, d

let rec affichage_bin_aux a = 
    (* Prend en argument un arbre non vide et renvoie un tuple 
       formé de :
        - la liste des lignes à afficher ;
        - la largeur totale à afficher ;
        - la position latérale du milieu de la racine dans l'affichage. 
        - la parité de la taille de la racine *)
    let sx = racine_bin a and g, d = enfants_bin a in
    let nx = String.length sx in
    match g, d with
        | Vide, Vide -> [sx], nx, nx / 2, nx mod 2
        | _, Vide    -> 
            let lstg, larg, posg, parg = affichage_bin_aux g in
            let ligne1 = String.make (posg + 1 + parg) ' ' ^ sx in
            let ligne2 = String.make (posg + parg) ' ' ^ "/" in
            ligne1 :: ligne2 :: lstg, max larg (posg + 1 + parg + nx), 
            posg + 1 + parg + nx / 2, nx mod 2
        | Vide, _    -> 
            let lstd, lard, posd, pard = affichage_bin_aux d in
            let nouv_posd = max posd (nx + 1) in
            let ligne1 = String.make (nouv_posd - 1 - nx) ' ' ^ sx in
            let ligne2 = String.make (nouv_posd - 1) ' ' ^ "\\" in
            let nouv_lstd = List.map (fun s -> String.make (nouv_posd - posd) ' ' ^ s) lstd in
            ligne1 :: ligne2 :: nouv_lstd, nouv_posd - posd + lard, 
            nouv_posd - 1 - nx + nx / 2, nx mod 2
        | _          -> 
            let lstg, larg, posg, parg = affichage_bin_aux g and
                lstd, lard, posd, pard = affichage_bin_aux d 
            in
            let nouv_posd = max (posd + larg + 1) (posg + parg + 2 + nx) in
            let ecart = nouv_posd - posg - parg in
            let debut = posg + parg + (ecart - nx) / 2 in
            let pos = debut + nx / 2 and fin = debut + nx in
            let ligne1 = String.make (posg + 1 + parg) ' ' ^ 
                         String.make (max 0 (debut - posg - 1 - parg)) '_' ^
                         sx ^
                         String.make (max 0 (nouv_posd - 1 - fin)) '_' in
            let ligne2 = String.make (posg + parg) ' ' ^ "/" ^
                         String.make (nouv_posd - posg - parg - 2) ' ' ^ "\\" in
            let rec fusion lg ld = match lg, ld with
                (* Fusionne deux listes de chaînes de caractères
                    en mettant le bon décalage pour la deuxième. *)
                | _, []      -> lg
                | [], s :: q -> (String.make (nouv_posd - posd) ' ' ^ s) :: fusion [] q
                | sg :: qg, sd :: qd -> 
                    let ng = String.length sg in
                    (sg ^ String.make (- ng + nouv_posd - posd) ' ' ^ sd) :: fusion qg qd 
            in
            ligne1 :: ligne2 :: fusion lstg lstd, lard + nouv_posd - posd,
            pos, nx mod 2

let afficher_arbre_bin a =
    print_newline ();
    match a with
       | Vide -> print_string "Vide\n";
       | a    -> let lst, lar, pos, par = affichage_bin_aux a in
                 List.iter print_endline lst

(*****************ARBRES QUELCONQUES*******************)
(* type arbre = Noeud of int * arbre list;; *)

let rec vers_lst = function
    | Vide       -> []
    | N(x, g, d) -> Noeud(x, vers_lst g) :: vers_lst d

let racine (Noeud(x, _)) = string_of_int x

let liste_enfants (Noeud(_, lst)) = lst

let rec affichage_aux a =
    (* Prend en argument un arbre non vide et renvoie un tuple 
       formé de :
        - la liste des lignes à afficher ;
        - la largeur totale à afficher. *)
    let sx = racine a and lst_enfants = liste_enfants a in
    let nx = String.length sx in
    let lst_aff = List.map affichage_aux lst_enfants in
    match lst_aff with
        | []           -> [sx], nx
        | [(lst, lar)] -> sx :: "|" :: lst, max lar nx
        | _            -> 
        let rec lgn2 = function
            (* Construit la deuxième ligne en fonction des
               largeurs des fils. *)
            | [] | [_]      -> failwith "Cas impossible"
            | [(_, lar); _] -> String.make lar '-' ^ "|"
            | (_, lar) :: q -> String.make lar '-' ^ "+" ^ lgn2 q 
        in
        let ligne2 = "|" ^ lgn2 lst_aff in
        let rec fusion l1 l2 decal = match l1, l2 with
            (* Fusionne deux listes de chaînes de caractères,
               la deuxième devant être décalée de decal. *)
            | _, []      -> l1
            | [], s :: q -> (String.make decal ' ' ^ s) :: fusion [] q decal
            | s1 :: q1, s2 :: q2 -> 
                let n1 = String.length s1 in
                (s1 ^ String.make (max 0 (decal - n1)) ' ' ^ s2) :: fusion q1 q2 decal 
        in
        let rec fusion_lst = function
            (* Fusionne récursivement les premières listes de
               chaînes de caractères, en calculant la largeur
               maximale. *)
            | []                            -> failwith "Cas impossible"
            | [(lst, lar)]                  -> [(lst, lar)]
            | (l1, lar1) :: (l2, lar2) :: q -> 
                fusion_lst ((fusion l1 l2 (lar1 + 1), lar1 + lar2 + 1) :: q) 
        in
        let (lst, lar) = List.hd (fusion_lst lst_aff) in
        sx :: ligne2 :: lst, max lar nx

let afficher_arbre a =
    print_newline ();
    let lst, lar = affichage_aux a in
    List.iter print_endline lst