open Tp13_outils

type image = int array array
type data = image array * int array

(* On commence par lire toutes les lignes une à une. Ensuite, on crée des tableaux
   de la bonne taille, et la fonction creer_image crée l'image et place l'étiquette
   et l'image dans les bonnes cases des tableaux. *)
let lire_donnees (fichier: string) : data =
    let canal = open_in fichier in
    let images_lst = ref [] in
    begin try 
        while true do
            images_lst := input_line canal :: !images_lst
        done
        with _ -> close_in canal
    end;
    let n = List.length !images_lst in
    let x = Array.make n [||] and y = Array.make n (-1) in
    let creer_image i str = 
        let lst = String.split_on_char ' ' str in
        y.(i) <- int_of_string (List.hd lst);
        let im = Array.make_matrix 8 8 (-1) in
        List.iteri (fun j x -> im.(j / 8).(j mod 8) <- int_of_string x) (List.tl lst);
        x.(i) <- im
    in
    List.iteri creer_image !images_lst;
    x, y

let xtrain, ytrain = lire_donnees "train"
let xtest, ytest = lire_donnees "test"

let _ =
    for i = 0 to 9 do
        afficher_chiffre xtrain.(i);
        Printf.printf "Étiquette : %d\n" ytrain.(i)
    done

(* On se contente de parcourir les coordonnées et d'ajouter leurs
   différences au carré. *)
let delta (im1: image) (im2: image) : int =
    let d = ref 0 in
    for i = 0 to 7 do
        for j = 0 to 7 do
            let x1 = im1.(i).(j) and
                x2 = im2.(i).(j) in
            d := !d + (x1 - x2) * (x1 - x2)
        done
    done;
    !d

(* On crée un tas de taille k et on y ajoute des couples
   (distance, étiquette). On extrait l'élément de distance
   maximale chaque fois que le tas contient plus de k éléments. 
   Finalement, on convertit le tas en tableau, en ne gardant que
   les étiquettes. *)
let plus_proches_voisins (k: int) ((xtrain, ytrain): data) (im: image) : int array =
    let tas = creer_tas () in
    for i = 0 to Array.length xtrain - 1 do
        let d = delta im xtrain.(i) in
        inserer d ytrain.(i) tas;
        if tas.taille > k then
           ignore (extraire_max tas)
    done;
    Array.map snd (tas_vers_tab tas)

(* On sait que les étiquettes sont comprises entre 0 et 9, donc on
   peut compter leurs occurrences dans un tableau de taille 10. On
   maintient à jour l'étiquette de plus grande occurrence au fur et
   à mesure. *)
let majoritaire (etiq: int array) : int =
    let k = Array.length etiq in
    let occ = Array.make 10 0 in
    let jmax = ref 0 in
    for i = 0 to k - 1 do
        let j = etiq.(i) in
        occ.(j) <- occ.(j) + 1;
        if occ.(j) > occ.(!jmax) then
           jmax := j
    done;
    !jmax

(* Il suffit de combiner les deux fonctions précédentes. *)
let kppv (k: int) (train: data) (im: image) =
    majoritaire (plus_proches_voisins k train im)

(* Pour chaque donnée de test, on calcule l'image obtenue par KPPV, et
   on incrémente la bonne case de la matrice. *)
let matrice_confusion (k: int) (train: data) ((xtest, ytest): data) : int array array =
    let confu = Array.make_matrix 10 10 0 in
    let n = Array.length xtest in
    for i = 0 to n - 1 do
        let j1 = ytest.(i) and 
            j2 = kppv k train xtest.(i) in
        confu.(j1).(j2) <- confu.(j1).(j2) + 1
    done;
    confu

(* Il suffit de faire le rapport entre la somme des coefficients
   hors de la diagonale et la somme des coefficients. *)
let taux_erreur (confu: int array array) : float =
    let erreurs = ref 0 and total = ref 0 in
    for i = 0 to 9 do
        for j = 0 to 9 do
            if i <> j then erreurs := !erreurs + confu.(i).(j);
            total := !total + confu.(i).(j)
        done
    done;
    float_of_int !erreurs /. (float_of_int !total)

let tests_kppv =
    List.iter (fun k -> 
        Printf.printf "Le taux d'erreur pour k = %d est %f\n" k
            (taux_erreur (matrice_confusion k (xtrain, ytrain) (xtest, ytest))))
        [1; 3; 7]

type rgb = int array
type image_couleur = rgb array array

(* Comme précédemment, mais sur un tableau de taille 3. *)
let delta_rgb (p1: rgb) (p2: rgb) : int =
    let dr = p1.(0) - p2.(0) and
        dg = p1.(1) - p2.(1) and
        db = p1.(2) - p2.(2) in
    dr * dr + dg * dg + db * db

(* On crée le tableau des barycentres, puis pour chaque case, on tire
   un pixel au hasard, qu'on copie dans la bonne case. *)
let selection_bary (k: int) (im: image_couleur) : rgb array =
    Random.self_init ();
    let bary = Array.make k [||] in
    let n = Array.length im and m = Array.length im.(0) in
    for i = 0 to k - 1 do
        let j = Random.int (n * m) in
        bary.(i) <- Array.copy im.(j / m).(j mod m)
    done;
    bary

(* Une simple recherche de minimum. *)
let bary_plus_proche (bary: rgb array) (p: rgb) : int =
    let imin = ref 0 in
    for i = 1 to Array.length bary - 1 do
        if delta_rgb bary.(i) p < delta_rgb bary.(!imin) p then
            imin := i
    done;
    !imin

(* On garde en mémoire un booléen qu'on met à false si on trouve un changement.
   On parcourt alors chaque pixel et on cherche le barycentre le plus proche. *)
let maj_classes (im: image_couleur) (bary: rgb array) (classes: int array array) : bool =
    let fini = ref true in
    for i = 0 to Array.length im - 1 do
        for j = 0 to Array.length im.(0) - 1 do
            let ib = bary_plus_proche bary im.(i).(j) in
            if ib <> classes.(i).(j) then fini := false;
            classes.(i).(j) <- ib
        done
    done;
    !fini

let ( ++ ) (p1: rgb) (p2: rgb) : rgb = [|p1.(0) + p2.(0); p1.(1) + p2.(1); p1.(2) + p2.(2)|]

(* Pour chacune des fonctions, on calque sur le modèle précédent. *)
let ( -- ) (p1: rgb) (p2: rgb) : rgb = [|p1.(0) - p2.(0); p1.(1) - p2.(1); p1.(2) - p2.(2)|]

let ( ** ) (p: rgb) (a: int) : rgb = [|p.(0) * a; p.(1) * a; p.(2) * a|]

let ( // ) (p: rgb) (a: int) : rgb = [|p.(0) / a; p.(1) / a; p.(2) / a|]

(* On garde en mémoire un tableau des tailles de classes, et on crée un nouveau 
   tableau de barycentres. Attention à ne pas faire Array.make k [|0;0;0|] au 
   risque d'avoir de la liaison de données ! Ensuite, on parcourt chaque pixel
   et on l'ajoute au nouveau barycentre de l'indice de sa classe. Finalement,
   on divise chaque nouveau barycentre par le cardinal de la classe si elle est
   non vide. *)
let maj_bary (im: image_couleur) (bary: rgb array) (classes: int array array) : unit =
    let k = Array.length bary in
    let tailles = Array.make k 0 in
    let nouv_bary = Array.make_matrix k 3 0 in
    for i = 0 to Array.length im - 1 do
        for j = 0 to Array.length im.(0) - 1 do
            let ib = classes.(i).(j) in
            tailles.(ib) <- 1 + tailles.(ib);
            nouv_bary.(ib) <- nouv_bary.(ib) ++ im.(i).(j)
        done
    done;
    for i = 0 to k - 1 do
        if tailles.(i) > 0 then
            bary.(i) <- nouv_bary.(i) // tailles.(i)
    done

(* On crée le tableau des barycentres et un tableau de classes. Ensuite, tant que
   les classes n'ont pas convergé et que le compteur n'est pas trop grand, on met
   à jour les classes et les barycentres. *)
let kmoyennes (k: int) (im: image_couleur) : rgb array * int array array =
    let bary = selection_bary k im in
    let n = Array.length im and m = Array.length im.(0) in
    let classes = Array.make_matrix n m (-1) in
    let compteur = ref 0 in
    while not (maj_classes im bary classes) && !compteur < 100 do
        maj_bary im bary classes;
        incr compteur
    done;
    bary, classes

(* On se contente de créer une nouvelle image et d'attribuer à chaque pixel le barycentre
   de sa classe. *)
let reconstruire (im: image_couleur) (bary: rgb array) (classes: int array array) : image_couleur =
    let n = Array.length im and m = Array.length im.(0) in
    let nouv_im = Array.make_matrix n m [||] in
    for i = 0 to n - 1 do
        for j = 0 to m - 1 do
            nouv_im.(i).(j) <- Array.copy bary.(classes.(i).(j))
        done
    done;
    nouv_im

let tests_kmoyennes =
    let mp = ouvrir_image_couleurs "martin_pecheur.png" in
    List.iter (fun k -> 
        let bary, classes = kmoyennes k mp in
        let im = reconstruire mp bary classes in
        sauvegarder_image_couleurs im ("martin_pecheur" ^ (string_of_int k) ^ ".png"))
        [4(*; 8; 16*)]

(* Or commence par créer une copie de l'image. Ensuite, en parcourant les
   pixels dans le bon ordre, on attribue au pixel son barycentre le plus
   proche, puis on diffuse l'erreur sur chacun des 4 pixels voisins pas
   encore vus. *)
let floyd_steinberg (im: image_couleur) (bary: rgb array) : image_couleur =
    let n = Array.length im and m = Array.length im.(0) in
    let nouv_im = Array.make_matrix n m [||] in
    for i = 0 to n - 1 do
        for j = 0 to m - 1 do
            nouv_im.(i).(j) <- im.(i).(j)
        done
    done;
    for i = 0 to n - 1 do
        for j = 0 to m - 1 do
            let p = nouv_im.(i).(j) in
            nouv_im.(i).(j) <- Array.copy bary.(bary_plus_proche bary p);
            let erreur = im.(i).(j) -- nouv_im.(i).(j) in
            let diffuser (di, dj, coeff) = 
                if i + di < n && j + dj >= 0 && j + dj < m then
                    nouv_im.(i + di).(j + dj) <- nouv_im.(i + di).(j + dj) ++ ((erreur ** coeff) // 16)
            in
            List.iter diffuser [(0, 1, 7); (1, -1, 3); (1, 0, 5); (1, 1, 1)]
        done
    done;
    nouv_im

let tests_diffusion =
    let mp = ouvrir_image_couleurs "martin_pecheur.png" in
    List.iter (fun k -> 
        let bary, classes = kmoyennes k mp in
        let im = reconstruire mp bary classes in
        sauvegarder_image_couleurs im ("martin_pecheur" ^ (string_of_int k) ^ ".png");
        let im_fs = floyd_steinberg mp bary in
        sauvegarder_image_couleurs im_fs ("martin_pecheur_FS" ^ (string_of_int k) ^ ".png");
        )
        [4; (*8; 16*)];
    let mpnb = ouvrir_image_gris "martin_pecheur_nb.png" in
    ()
