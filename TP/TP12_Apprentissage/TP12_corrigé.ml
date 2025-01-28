(* #use "TP12_outils.ml";; *)

let delta im1 im2 =
    let d = ref 0 in
    for i = 0 to 27 do
        for j = 0 to 27 do
            let x1 = im1.(i).(j) and
                x2 = im2.(i).(j) in
            d := !d + (x1 - x2) * (x1 - x2)
        done
    done;
    !d

let delta_manhattan im1 im2 =
    let d = ref 0 in
    for i = 0 to 27 do
        for j = 0 to 27 do
            let x1 = im1.(i).(j) and
                x2 = im2.(i).(j) in
            d := !d + abs (x1 - x2)
        done
    done;
    !d  
    
(* On crée un tas de taille k et on y ajoute des couples
   (distance, étiquette). On extrait l'élément de distance
   maximale chaque fois que le tas contient plus de k éléments. 
   Finalement, on convertit le tas en tableau, en ne gardant que
   les étiquettes. *)
let plus_proches_voisins k donnees im =
    let imgs, etqs = donnees and
        tas = creer_tas () in
    for i = 0 to Array.length imgs - 1 do
        let d = delta im imgs.(i) in
        inserer d etqs.(i) tas;
        if tas.taille > k then
           ignore (extraire_max tas)
    done;
    Array.map snd (tas_vers_tab tas)

(* On sait que les étiquettes sont comprises entre 0 et 9, donc on
   peut compter leurs occurrences dans un tableau de taille 10. On
   maintient à jour l'étiquette de plus grande occurrence au fur et
   à mesure. *)
let majoritaire etiquettes =
    let k = Array.length etiquettes in
    let occ = Array.make 10 0 in
    let jmax = ref 0 in
    for i = 0 to k - 1 do
        let j = etiquettes.(i) in
        occ.(j) <- occ.(j) + 1;
        if occ.(j) > occ.(!jmax) then
           jmax := j
    done;
    !jmax

(* Il suffit de combiner les deux fonctions précédentes. *)
let kppv k donnees im =
    majoritaire (plus_proches_voisins k donnees im)

(* Pour chaque donnée de test, on calcule l'image obtenue par KPPV, et
   on incrémente la bonne case de la matrice. *)
let matrice_confusion k train test =
    let mat = Array.make_matrix 10 10 0 in
    let imgs_test, etqs_test = test in
    let n = Array.length imgs_test in
    for i = 0 to n - 1 do
        let j1 = etqs_test.(i) and 
            j2 = kppv k train imgs_test.(i) in
        mat.(j1).(j2) <- mat.(j1).(j2) + 1
    done;
    mat

(* Un petit alias pour raccourcir le nom de fonction. *)
let foi = float_of_int

(* Il suffit de faire le rapport entre la somme des coefficients
   hors de la diagonale et la somme des coefficients. *)
let taux_erreur mat =
    let erreurs = ref 0 and
        total = ref 0 in
    for i = 0 to 9 do
        for j = 0 to 9 do
            if i <> j then erreurs := !erreurs + mat.(i).(j);
            total := !total + mat.(i).(j)
        done
    done;
    foi !erreurs /. (foi !total)

(* Découpe un jeu de données. *)
let sub_data data n =
    let imgs, etqs = data in
    Array.sub imgs 0 n, Array.sub etqs 0 n

let sub_train = sub_data train 2000 and sub_test = sub_data test 400

(* Une fonction de test, avec chronomètre. *)
let tester k train test =
    let deb = Sys.time () in
    Printf.printf "taux d'erreur : %f\n"
        (taux_erreur (matrice_confusion k train test));
    Printf.printf "Temps de calcul : %f\n"
        (Sys.time () -. deb)

let _ = tester 3 sub_train sub_test

(* Parcourt chaque pixel pour le conserver dans la liste si besoin. *)
let convertir im =
    let lst = ref [] in
    for i = 0 to 27 do
        for j = 0 to 27 do
            if im.(i).(j) > 10 then lst := (i, j) :: !lst
        done
    done;
    {im = im; sign = !lst}

(* On parcourt la première liste en prenant en compte chaque pixel. Puis
   on parcourt la deuxième liste en ne prenant en compte que les pixels de
   la deuxième image qui ne sont pas significatifs dans la première. *)
let delta2 a b =
    let d = ref 0 in
    let traiter1 (i, j) =
        let x1 = a.im.(i).(j)
        and x2 = b.im.(i).(j) in
        d := !d + (x1 - x2) * (x1 - x2) 
    in
    let traiter2 (i, j) =
        if a.im.(i).(j) <= 10 then traiter1 (i, j) 
    in
    List.iter traiter1 a.sign;
    List.iter traiter2 b.sign;
    !d

(* On réécrit les fonctions en changeant juste la fonction de distance. *)
let plus_proches_voisins2 k donnees im =
    let imgs, etqs = donnees and
        tas = creer_tas () in
    for i = 0 to Array.length imgs - 1 do
        let d = delta2 im imgs.(i) in
        inserer d etqs.(i) tas;
        if tas.taille > k then
           ignore (extraire_max tas)
    done;
    Array.map snd (tas_vers_tab tas)

let knn2 k donnees im =
    majoritaire (plus_proches_voisins2 k donnees im)

let matrice_confusion2 k train test =
    let mat = Array.make_matrix 10 10 0 in
    let imgs_test, etqs_test = test in
    let n = Array.length imgs_test in
    for i = 0 to n - 1 do
        let j1 = etqs_test.(i) and 
            j2 = knn2 k train imgs_test.(i) in
        mat.(j1).(j2) <- mat.(j1).(j2) + 1
    done;
    mat

(* Fonction pour convertir tout un jeu de données. *)
let convertir_data data =
    let imgs, etqs = data in
    Array.map convertir imgs, etqs

(* On commence par convertir les données avant de faire les tests, car
   les données d'entraînement nécessiteraient d'être converties pour chaque
   donnée de test sinon. *)
let tester2 k train test =
    let train2 = convertir_data train and 
        test2 = convertir_data test in
    let deb = Sys.time () in
    Printf.printf "taux d'erreur : %f\n"
        (taux_erreur (matrice_confusion2 k train2 test2));
    Printf.printf "Temps de calcul : %f\n"
        (Sys.time () -. deb)

let _ = tester2 3 sub_train sub_test

exception Termine

(* Comme delta2, mais on lève une exception pour arrêter le calcul si la distance 
   devient supérieure au seuil. *)
let delta3 a b seuil =
    let d = ref 0 in
    let traiter1 (i, j) =
        let x1 = a.im.(i).(j)
        and x2 = b.im.(i).(j) in
        d := !d + (x1 - x2) * (x1 - x2);
        if !d > seuil then raise Termine;
    in
    let traiter2 (i, j) =
        if a.im.(i).(j) <= 127 then traiter1 (i, j) 
    in
    (try List.iter traiter1 a.sign;
        List.iter traiter2 b.sign
    with _ -> ());
    !d

(* Tant qu'il n'y a pas encore k éléments dans le tas, on calcule les distances
   avec delta2. Ensuite, on calcule avec delta3. *)
let plus_proches_voisins3 k donnees im =
    let imgs, etqs = donnees and
        tas = creer_tas () in
    for i = 0 to Array.length imgs - 1 do
        let d = 
            if tas.taille < k then delta2 im imgs.(i)
            else delta3 im imgs.(i) (fst (maximum tas))
        in
        inserer d etqs.(i) tas;
        if tas.taille > k then
           ignore (extraire_max tas)
    done;
    Array.map snd (tas_vers_tab tas)

(* Pas de changement pour la suite. *)
let knn3 k donnees im =
    majoritaire (plus_proches_voisins3 k donnees im)

let matrice_confusion3 k train test =
    let mat = Array.make_matrix 10 10 0 in
    let imgs_test, etqs_test = test in
    let n = Array.length imgs_test in
    for i = 0 to n - 1 do
        let j1 = etqs_test.(i) and 
            j2 = knn3 k train imgs_test.(i) in
        mat.(j1).(j2) <- mat.(j1).(j2) + 1
    done;
    mat

let tester3 k train test =
    let train3 = convertir_data train and 
        test3 = convertir_data test in
    let deb = Sys.time () in
    Printf.printf "taux d'erreur : %f\n"
        (taux_erreur (matrice_confusion3 k train3 test3));
    Printf.printf "Temps de calcul : %f\n"
        (Sys.time () -. deb)

let _ = tester3 3 sub_train sub_test