(*****************************************************)
(*Exercice 1*)

let rec liste_alea k = function
    (* Si la liste n'est pas de taille nulle, on génère
       un entier aléatoire et une liste de taille n - 1. *)
    | 0 -> []
    | n -> Random.int k :: liste_alea k (n - 1);;

let afficher_liste lst =
    (* Fonction qui affiche une liste d'entiers. *)
    print_string "[";
    let rec afficher = function
        | []     -> print_string "]\n";
        | [x]    -> print_int x; print_string "]\n";
        | x :: q -> print_int x; print_string "; "; afficher q in
    afficher lst;;

let rec length = function
    (* La liste vide est de taille nulle. La taille d'une
       liste quelconque est 1 de plus que sa queue. *)
    | []     -> 0
    | _ :: q -> 1 + length q;;

let rec mem x = function
    (* Si la liste est vide, l'élément est absent. Sinon,
       il est présent s'il est en tête ou dans la queue. *)
    | []     -> false
    | y :: q -> x = y || mem x q;;

let rec append l1 l2 = match l1 with
    (* On ne fait le filtrage que sur la première liste.
       Si elle est vide, on renvoie la deuxième, sinon on
       concatène sa queue avec l2 et on rajoute la tête. *)
    | []     -> l2
    | x :: q -> x :: append q l2;;
    
let rec map f = function
    (* On calcule l'image de la tête qu'on rajoute en
       tête de l'image de la queue. *)
    | []     -> []
    | x :: q -> f x :: map f q;;

let rec iter f = function
    (* Quasiment le même code que précédemment sauf qu'on
       ne renvoie pas une liste. *)
    | []     -> ()
    | x :: q -> f x; iter f q;;

let rec somme = function
    (* On utilise la convention que la liste vide est de
       somme nulle. On ajoute la valeur de la tête à la
       somme de la queue sinon. *)
    | []     -> 0
    | x :: q -> x + somme q;;
    
let rec maximum = function
    (* On traite à part les cas de la liste vide (erreur)
       et du singleton. Sinon, on compare la tête au
       maximum de la queue. *)
    | []     -> failwith "Liste vide"
    | [x]    -> x
    | x :: q -> max x (maximum q);;

let rec partition p = function
    (* On partitionne la queue et on rajoute la tête à
       l'une des deux listes, selon qu'elle vérifie le
       prédicat ou non. *)
    | []     -> [], []
    | x :: q -> let l1, l2 = partition p q in
                if p x then x :: l1, l2
                       else l1, x :: l2;;

(*         Tests              *)
Random.self_init ();
let lst = liste_alea 30 20 in

(print_string "La liste est : "; afficher_liste lst;
Printf.printf "La taille est : %d\n" (length lst);
Printf.printf "La somme est : %d\n" (somme lst);
Printf.printf "Le maximum est : %d\n" (maximum lst);
let l1, l2 = partition (fun x -> x mod 2 = 0) lst in
print_string "Les valeurs paires sont : "; afficher_liste l1; 
print_string "Les valeurs impaires sont : "; afficher_liste l2;

(* Quelques tests automatisés pour vérifier qu'il n'y a pas
   d'erreurs dans des cas particuliers. *)
for n = 0 to 20 do
    for k = 1 to 30 do
        let lst = liste_alea k n in
        assert (length lst = n);
        assert (n = 0 || maximum lst < k);
        assert (not (mem k lst));
        assert (n = 0 || mem (maximum lst) lst);
    done
done);;

(*****************************************************)
(*Exercice 2*)

let fibo n =
    (* On écrit une fonction auxiliaire qui prend en
       arguments deux valeurs a et b et un entier n et
       renvoie les n + 1 premiers termes de la suite en
       supposant f_0 = a et f_1 = b. *)
    let rec fibo_aux a b = function
        (* Si n = 0, on ne renvoie que [a]. Sinon, cela
           revient à calculer les n premiers termes en
           commençant avec f_1 = b et f_2 = f_0 + f_1
           et à rajouter f_0 = 0 en tête. *)
        | 0 -> [a]
        | n -> a :: fibo_aux b (a + b) (n - 1) in
    fibo_aux 0 1 n;;
    
(*         Tests              *)
let lst = fibo 60 in
Printf.printf "f_5 vaut bien 5 : %d\n" (List.nth lst 5);
Printf.printf "f_10 vaut bien 55 : %d\n" (List.nth lst 10);
Printf.printf "La valeur de f_60 est : %d\n" (List.nth lst 60);;

(*****************************************************)
(*Exercice 3*)

(* 
#use "/home/nathaniel/Nextcloud/Cours/MPI/TP/TP1-Révisions OCaml/affichage_arbres.ml";; 
open Affichage_arbres;;
*)

Random.self_init ();;

let arbre_bin_alea n =
    (* On écrit la fonction auxiliaire suggérée. *)
    let rec aux k = function
        (* Si l'arbre n'est pas vide, on choisit aléatoirement
           la taille de l'arbre gauche, on en déduit celle de
           l'arbre droit ainsi que les plages de valeurs des
           étiquettes. *)
        | 0 -> Vide
        | n -> let m = Random.int n in
               N(k + m, aux k m, aux (k + m + 1) (n - m - 1)) in
    aux 0 n;;

let rec taille_bin = function
    (* L'arbre vide est de taille nulle. Sinon on utilise la
       formule |N(g, d)| = 1 + |g| + |d|. *)
    | Vide       -> 0
    | N(_, g, d) -> 1 + taille_bin g + taille_bin d;;

let rec hauteur_bin = function
    (* Même principe avec h(N(g, d)) = 1 + max(h(g), h(d)). *)
    | Vide       -> -1
    | N(_, g, d) -> 1 + max (hauteur_bin g) (hauteur_bin d);;

let rec affiche_prefixe = function
    (* On affiche la racine, puis l'ordre préfixe de g,
       puis l'ordre préfixe de d. *)
    | Vide       -> ()
    | N(x, g, d) -> print_int x; print_string " ";
                    affiche_prefixe g; affiche_prefixe d;;

let rec infixe = function
    (* Une première version naïve consiste à faire des
       concaténations entre les parcours infixes des fils. *)
    | Vide       -> []
    | N(x, g, d) -> infixe g @ x :: infixe d;;

(* Malheureusement, cette version a une complexité quadratique
   dans le pire cas. Il faut utiliser une fonction auxiliaire
   pour améliorer la complexité. *)

let infixe a =
    (* On utilise une fonction auxiliaire qui prend en argument
       une liste acc et un arbre a et renvoie infixe a @ acc *)
    let rec infixe_aux acc = function
        (* Le principe est que infixe N(x, g, d) @ acc =
           infixe g @ x :: infixe d @ acc. On peut donc
           calculer infixe d @ acc récursivement, rajouter x
           puis calculer infixe g @ [le résultat] récursivement. *)
        | Vide       -> acc
        | N(x, g, d) -> infixe_aux (x :: infixe_aux acc d) g in
    infixe_aux [] a;;

(*         Tests              *)
let a = arbre_bin_alea 30 in
(afficher_arbre_bin a;
Printf.printf "La taille est : %d\n" (taille_bin a);
Printf.printf "La hauteur est : %d\n" (hauteur_bin a);
print_string "Le parcours préfixe est : "; affiche_prefixe a;
print_newline ());

(* Quelques tests automatisés pour vérifier qu'il n'y a pas
   d'erreurs dans des cas particuliers. *)
for n = 0 to 50 do
    let a = arbre_bin_alea n in
    assert (taille_bin a = n);
    assert (infixe a = List.init n (fun i -> i));
done;;

(*****************************************************)
(*Exercice 4*)
let rec vers_lst = function
    | Vide       -> []
    | N(x, g, d) -> Noeud(x, vers_lst g) :: vers_lst d;;

let arbre_alea n =
    let a = arbre_bin_alea (n - 1) in
    Noeud(n - 1, vers_lst a);;

(* Il y a plusieurs façons d'écrire les fonctions sur les
   arbres d'arité quelconque. Soit une fonction directement
   récursivement où on filtre sur la liste des fils, soit on
   écrit une fonction auxiliaire qui prend en argument
   directement une liste d'arbre et on l'utilise avec les bons
   arguments. *)
let rec taille (Noeud(x, lst)) = match lst with
    (* Si l'arbre a possède au moins un fils f, la taille de a
       est celle de f + celle de l'arbre a où on a supprimé f. *)
    | []     -> 1
    | f :: q -> taille f + taille (Noeud(x, q));;

let taille a =
    (* On écrit une fonction auxiliaire qui prend en argument
       une liste d'arbres et renvoie la somme des tailles des
       arbres de la liste. *)
    let rec taille_aux = function
        | []                 -> 0
        | Noeud(x, lst) :: q -> 1 + taille_aux lst + taille_aux q in
    taille_aux [a];;

let rec taille (Noeud(x, lst)) =
    (* On peut aussi écrire une fonction directement récursive
       en utilisant les fonctionnelles correctement. Ça ne
       rend pas le code plus facile à comprendre… Ici, l'appel
       à List.fold_left permet de calculer la somme des entiers
       d'une liste. *)
    1 + List.fold_left (fun a b -> a + b) 0 (List.map taille lst);;

let rec taille (Noeud(x, lst)) =
    (* Même idée que précédemment sans le map. *)
    1 + List.fold_left (fun a b -> a + taille b) 0 lst;;

let rec hauteur (Noeud(x, lst)) = match lst with
    (* Même principe en adaptant la formule. *)
    | []     -> 0
    | f :: q -> max (1 + hauteur f) (hauteur (Noeud(x, q)));;

let suffixe a =
    (* On écrit une fonction auxiliaire qui prend en argument
       une liste acc et une liste d'arbre et renvoie la
       concaténation des parcours suffixes de la liste d'arbres
       concaténée avec acc. *)
    let rec suffixe_aux acc = function
        | []                 -> acc
        | Noeud(x, lst) :: q -> suffixe_aux (x :: suffixe_aux acc q) lst in
    suffixe_aux [] [a];;

let rec bin_vers_arbre = function
    (* On distingue selon le nombre de fils de l'arbre. On
       convertit ces fils et on reconstruit le nouvel arbre. *)
    | Vide             -> failwith "Cas impossible"
    | N(x, Vide, Vide) -> Noeud(x, [])
    | N(x, a, Vide) | N(x, Vide, a) -> Noeud(x, [bin_vers_arbre a])
    | N(x, g, d)       -> let nouv_g = bin_vers_arbre g and
                              nouv_d = bin_vers_arbre d in
                          Noeud(x, [nouv_g; nouv_d]);;

(*         Tests              *)
let a = arbre_alea 30 in
(afficher_arbre a;
Printf.printf "La taille est : %d\n" (taille a);
Printf.printf "La hauteur est : %d\n" (hauteur a););

(* Quelques tests automatisés pour vérifier qu'il n'y a pas
   d'erreurs dans des cas particuliers. *)
for n = 1 to 50 do
    let a = arbre_alea n in
    assert (taille a = n);
    assert (suffixe a = List.init n (fun i -> i));
done;;

(*****************************************************)
(*Exercice 5*)

let tab_alea k n =
    (* On crée un tableau de zéros qu'on remplit avec
       des valeurs aléatoires. *)
    let tab = Array.make n 0 in
    for i = 0 to n - 1 do
        tab.(i) <- Random.int k
    done;
    tab;;

let afficher_tab tab =
    (* Fonction qui affiche un tableau d'entiers. *)
    print_string "[|";
    let n = Array.length tab in
    for i = 0 to n - 2 do
        print_int tab.(i); print_string "; "
    done;
    if n > 0 then print_int tab.(n - 1);
    print_string "|]\n";;    
    
let init n f =
    (* On crée un tableau avec l'image de zéro et on
       modifie les autres valeurs en conséquence. *)
    let tab = Array.make n (f 0) in
    for i = 1 to n - 1 do
        tab.(i) <- f i
    done;
    tab;;

let minimum tab =
    (* Si le tableau est vide, on renvoie une erreur.
       Sinon, on utilise une référence pour garder en
       mémoire le minimum en parcourant toutes les cases
       du tableau. *)
    let n = Array.length tab in
    if n = 0 then failwith "Tableau vide" else begin
       let mini = ref tab.(0) in
       for i = 1 to n - 1 do
           mini := min !mini tab.(i)
       done;
       !mini
    end;;

let tri_insertion tab =
    let n = Array.length tab in
    for i = 1 to n - 1 do
        (* Pour chaque valeur de i, on insère tab.(i)
           à la bonne position entre 0 et i. *)
        let j = ref i and x = tab.(i) in
        while !j > 0 && tab.(!j - 1) > x do
            (* Tant que x n'est pas à la bonne position,
               on décale la valeur d'un cran. *)
            tab.(!j) <- tab.(!j - 1);
            decr j
        done;
        tab.(!j) <- x
    done;;

let knuth tab =
    (* On applique ce qui est décrit… *)
    let n = Array.length tab in
    for i = n - 1 downto 1 do
        let k = Random.int (i + 1) in
        let x = tab.(i) in
        tab.(i) <- tab.(k);
        tab.(k) <- x
    done;;

let fold_left f a tab =
    (* Il suffit de suivre la formule. *)
    let n = Array.length tab in
    let ret = ref a in
    for i = 0 to n - 1 do
        ret := f !ret tab.(i)
    done;
    !ret;;

(*         Tests              *)
let tab = tab_alea 30 20 in
afficher_tab tab;
(Printf.printf "Le minimum est : %d\n" (minimum tab);
Printf.printf "La somme est : %d\n" (fold_left (fun a b -> a + b) 0 tab););
tri_insertion tab;
afficher_tab tab;
knuth tab;
afficher_tab tab;;

(*****************************************************)
(*Exercice 6*)

let convertir lst = 
    (* On commence par suivre l'indication. *)
    let rec nbr_vers_chf = function
        (* Si la tête est un chiffre, on se contente de
           transformer la queue. Sinon, on coupe la tête
           en deux entre le chiffre des unités et le reste. *)
        | []                 -> []
        | x :: q when x < 10 -> x :: nbr_vers_chf q
        | x :: q             -> nbr_vers_chf (x / 10 :: x mod 10 :: q) in
    (* La fonction suivante prend en argument un entier acc et
       une liste de chiffre lst et renvoie l'entier qui s'écrit
       avec les chiffres de acc suivi des chiffres de lst. *)
    let rec chf_vers_int acc = function
        (* Pour chaque élément de la liste, on multiplie acc par
           10 et on ajoute l'élément. *)
        | []     -> acc
        | x :: q -> chf_vers_int (10 * acc + x) q in
    chf_vers_int 0 (nbr_vers_chf lst);;

(*         Tests              *)
let lst = liste_alea 2000 4 in
afficher_liste lst;
print_int (convertir lst);
print_newline ();;

(* Il faut faire attention aux dépassements d'entiers si le
   résultat contient trop de chiffres. *)

(*****************************************************)
(*Exercice 7*)

type symb = I of int | Op of (int -> int -> int)
and expr = symb list;;

let postfixe ex =
    (* Le principe est le suivant : on utilise une
       pile gardant en mémoire les résultats. Lorsqu'on
       lit un entier dans l'expression, on l'empile.
       Si on lit un opérateur, on dépile les deux derniers
       résultats, on fait l'opération, et on empile la
       valeur obtenue. Une fois l'expression lue, il doit
       ne rester qu'un seul résultat dans la pile. *)
    let rec postfixe_aux pile ex = match pile, ex with
        | [res], []     -> res
        | _, I n :: qex -> postfixe_aux (n :: pile) qex
        | r1 :: r2 :: q, Op op :: qex -> postfixe_aux (op r2 r1 :: q) qex
        | _ -> failwith "Expression mal formée" in
    postfixe_aux [] ex;;

(*         Test              *)
assert (25 = postfixe [I 3; I 5; I 4; Op ( + ); Op ( * ); I 2; Op ( - )]);;

(*****************************************************)
(*Exercice 8*)

let arbre_vers_bin a =
    (* On écrit une fonction qui prend en argument une liste
       d'arbre et renvoie un arbre binaire associé. *)
    let rec lst_vers_bin = function
        | [] -> Vide
        | Noeud(x, lst) :: q -> N(x, lst_vers_bin lst, lst_vers_bin q) in
    lst_vers_bin [a];;

(*         Tests              *)
let a = arbre_alea 30 in
afficher_arbre a;
afficher_arbre_bin (arbre_vers_bin a);;

