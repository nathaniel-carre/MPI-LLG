(* Le chemin suivant est à remplacer par le chemin du dossier où vous 
   enregistrerez les 4 fichiers nécessaires aux tests et à l'apprentissage. 
   Mettre une chaîne de caractère vide peut suffire dans un environnement linux. *)
let chemin = "/chemin/a/modifier/";;

type image = {matrice : int array array; etiquette : int};;

let ouvrir set =
    let images = open_in_bin (chemin ^ set ^ "-images.idx3-ubyte")
    and etiquettes = open_in_bin (chemin ^ set ^ "-labels.idx1-ubyte") in
    ignore (input_binary_int images);
    let nb = input_binary_int images in
    let hauteur = input_binary_int images in
    let largeur = input_binary_int images in
    ignore (really_input_string etiquettes 8);
    let data = Array.make nb {matrice = [||]; etiquette = 0} in
    for i = 0 to nb - 1 do
        data.(i) <- {matrice = Array.make_matrix hauteur largeur 0;
                     etiquette = input_byte etiquettes};
        for lgn = 0 to hauteur - 1 do
            for col = 0 to largeur - 1 do
                data.(i).matrice.(lgn).(col) <- input_byte images
            done
        done
   done;
   data;;

let entrainement = ouvrir "train" and test = ouvrir "t10k";;

type ('a, 'b) arbre = Nil | Node of 'a * 'b * ('a, 'b) arbre list;;
type ('a, 'b) tas = {mutable taille : int; mutable data : ('a, 'b) arbre};;

let creer_tas () = {taille = 0; data = Nil};;

let fusion t1 t2 = match t1, t2 with
    | Nil, t | t, Nil                  -> t
    | Node(p1, x, l1), Node(p2, y, l2) -> if p1 < p2 then Node(p2, y, t1 :: l2)
                                                     else Node(p1, x, t2 :: l1);;

let inserer p x t = 
    t.taille <- t.taille + 1;
    t.data <- fusion (Node(p, x, [])) t.data;;

let maximum t = match t.data with
    | Nil     -> failwith "Tas vide."
    | Node(p, x, _) -> (p, x);;

let rec fusion_liste = function
    | []            -> Nil
    | [t]           -> t
    | t1 :: t2 :: q -> fusion (fusion t1 t2) (fusion_liste q);;

let extraire_max t = match t.data with
    | Nil        -> failwith "Tas vide."
    | Node(p, x, l) -> t.taille <- t.taille - 1; t.data <- fusion_liste l; p, x;;

let tas_vers_tab t =
    let n = t.taille in
    if n = 0 then [||] else begin
       let tab = Array.make n (maximum t) in
       let rec convertir i = function
           | Nil           -> tab
           | Node(p, x, l) -> tab.(i) <- (p, x);
                              convertir (i - 1) (fusion_liste l) in
       convertir (n - 1) t.data end;;

(* Les 2 lignes suivantes sont à décommenter pour les versions
   récentes de OCaml. *)
(* #use "topfind";;
#require "graphics";; *)

(* La ligne suivante est à décommenter pour les anciennes 
   versions de WinCaml *)
(* #load "graphics.cma";; *)

(* Le code suivant permet d'afficher une image : la fonction prend
   en argument une image (au sens du type défini précédemment) et un
   entier (correspondant à une taille de pixel) et affiche l'image
   dans une fenêtre graphique. *)
   
(*
open Graphics;;

let afficher im pixel =
    let h, l = Array.length im, Array.length im.(0) in
    open_graph "";
    for lgn = 0 to h - 1 do
        for col = 0 to l - 1 do
            let c = im.(lgn).(col) in
            set_color (rgb c c c);
            fill_rect (pixel * col) (pixel * (h - lgn - 1)) pixel pixel;
        done
    done;;*)


(* Le code suivant permet de lire une image PNG et de la transformer
   en matrice correspondant à cette image en niveaux de gris. On pourra
   l'utiliser pour tester de nouvelles images 28 × 28 si on le souhaite
   (attention, il faut que camlimages soit installé, donc ça risque de ne
   pas marcher avec WinCaml) *)

(*
#use "topfind";;
#require "camlimages.png";;

let niveau_de_gris r g b = 
    (2126*r + 7152*g + 722*b)/10000;;

let lire_image fichier =
    (* fichier est une chaîne de caractères correspondant au nom de l'image
       à lire (au format png) *)
    let Images.Rgb24 im = Png.load_as_rgb24 (chemin^fichier) [] in
    let h = im.height and l = im.width in
    let s = Rgb24.dump im in
    let image = Array.make_matrix h l 0 in
    for lgn = 0 to h - 1 do
        for col = 0 to l - 1 do
            let i = 3 * (lgn * l + col) in
            let r = Bytes.get_uint8 s i 
            and g = Bytes.get_uint8 s (i + 1) 
            and b = Bytes.get_uint8 s (i + 2) in
            image.(lgn).(col) <-  niveau_de_gris r g b
        done
    done;
    image;;*)