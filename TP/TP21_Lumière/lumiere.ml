(* Question 1 *)

(* Création d'un tableau contenant les uk pour k de 0 à n *)
let tab_un u0 n =
    let tab = Array.make (n + 1) u0 in
    for i = 1 to n do
        tab.(i) <- (569 * tab.(i - 1)) mod 2_424_259
    done;
    tab

let q1 u0 = 
    let traiter n = 
        let tab = tab_un u0 n in
        Printf.printf "%d\n" (tab.(n) mod 10000)
    in
    List.iter traiter [1; 128; 2024; 123_456]

(* Question 2 *)

(* Création de la grille sous forme de matrice de 0/1 *)
let grille u0 n m p =
    let indice i j = n*n + 89*m + 71*p + n*i + j in
    let tab = tab_un u0 (indice n n) in
    let g = Array.make_matrix n n 0 in
    for i = 0 to n - 1 do
        for j = 0 to n - 1 do
            if tab.(indice i j) mod p < m then
                g.(i).(j) <- 1
        done
    done;
    g

(* Calcul de la somme de contrôle utilisée plusieurs fois *)
let somme_controle g =
    let n = Array.length g in
    let s = ref 0 in
    for i = 0 to n - 1 do
        for j = 0 to n - 1 do
            if g.(i).(j) = 1 then
                s := !s + i + j
        done
    done;
    !s mod 10000

let q2 u0 = 
    let traiter (n, m, p) = 
        let g = grille u0 n m p in
        Printf.printf "%d\n" (somme_controle g)
    in
    List.iter traiter [(3, 1, 4); (10, 3, 4); (100, 2, 5)]

(* Question 3 *)

let distance g1 g2 =
    let n = Array.length g1 in
    let d = ref 0 in
    for i = 0 to n - 1 do
        for j = 0 to n - 1 do
            if g1.(i).(j) <> g2.(i).(j) then incr d
        done
    done;
    !d

let q3 u0 = 
    let traiter (n, m1, p1, m2, p2) = 
        let g1 = grille u0 n m1 p1 and
            g2 = grille u0 n m2 p2 in
        Printf.printf "%d\n" (distance g1 g2)
    in
    List.iter traiter [(3, 1, 4, 1, 2); (10, 3, 4, 1, 2); (100, 2, 5, 3, 7)]

(* Question 4 *)

(* Renvoie les cellules voisines (y compris elle-même) qui sont dans les
   bornes de la matrice *)
let voisinage i j n =
    let test (i', j') =
        0 <= i' && i' < n && 0 <= j' && j' < n
    in
    List.filter test [(i, j); (i - 1, j); (i + 1, j); (i, j - 1); (i, j + 1)]

(* Effectue l'action d'appuyer sur une cellule *)
let appuyer g i j =
    let n = Array.length g in
    let changer (i', j') =
        g.(i').(j') <- 1 - g.(i').(j')
    in
    List.iter changer (voisinage i j n)

(* Étant donné une grille de départ et la matrice des cellules à appuyer,
   joue la partie (l'ordre n'importe pas) *)
let jouer_partie g mat =
    let n = Array.length g in
    for i = 0 to n - 1 do
        for j = 0 to n - 1 do
            if mat.(i).(j) = 1 then appuyer g i j
        done
    done

(* Étant donné une matrice des cellules à appuyer, calcule la matrice
   suivante selon l'ordre lexicographique (pour toute les parcourir) *)
let incrementer mat =
    let n = Array.length mat in
    let k = ref 0 in
    while !k < n * n do
        let i, j = !k / n, !k mod n in
        if mat.(i).(j) = 0 then
            (mat.(i).(j) <- 1; k := n * n + 1)
        else
            (mat.(i).(j) <- 0; incr k)
    done;
    !k = n * n + 1

(* Algorithme qui part d'une matrice de zéros, puis parcourt toutes les
   matrices de cellules à appuyer possibles (en incrémentant à chaque fois)
   et garde en mémoire la meilleure. *)
let bruteforce g =
    let n = Array.length g in
    let nul = Array.make_matrix n n 0 in
    let mat = Array.make_matrix n n 0 in
    let best_d = ref (distance g nul) in
    let best_somme = ref (somme_controle mat) in
    while (incrementer mat) do
        jouer_partie g mat;
        let d = distance g nul in
        let s = somme_controle mat in
        if (d, -s) < (!best_d, - !best_somme) then
            (best_d := d; best_somme := s;);
        jouer_partie g mat
    done;
    !best_somme

let q4 u0 = 
    let traiter (n, m, p) = 
        let g = grille u0 n m p in
        Printf.printf "%d\n" (bruteforce g)
    in
    List.iter traiter [(3, 1, 2); (4, 1, 4); (4, 3, 4)]

(* Question 5 *)

(* Calcule le différentiel de distance lorsqu'on appuie sur
   une cellule (pour éviter de recalculer toute la distance
   à chaque fois) *)
let maj_dist g i j =
    let nouv_d = ref 0 in
    let traiter (i', j') = 
        if g.(i').(j') = 1 then decr nouv_d
        else incr nouv_d;
    in
    List.iter traiter (voisinage i j (Array.length g));
    !nouv_d

(* Calcule le meilleur coup à jouer, c'est-à-dire celui qui
   minimise le différentiel de distance, parmi les coups non
   déjà joués (donnés par la matrice vus) *)
let meilleur_coup g vus =
    let n = Array.length g in
    let best_d = ref 6 and best_ij = ref (-1, -1) in
    for i = n - 1 downto 0 do
        for j = n - 1 downto 0 do
            if vus.(i).(j) = 0 then begin
                let d = maj_dist g i j in
                if d < !best_d then
                    (best_d := d; best_ij := (i, j))
            end
        done
    done;
    !best_ij

let glouton g =
    let n = Array.length g in
    let vus = Array.make_matrix n n 0 in
    let best_d = ref 0 and
        best_somme = ref 0 and
        d_cur = ref 0 in
    for _ = 0 to n * n - 1 do
        let (i, j) = meilleur_coup g vus in
        d_cur := !d_cur + maj_dist g i j;
        appuyer g i j;
        vus.(i).(j) <- 1;        
        if !d_cur < !best_d then begin
            best_d := !d_cur;
            best_somme := somme_controle vus
        end
    done;
    !best_somme

let q5 u0 =
    let traiter (n, m, p) = 
        let g = grille u0 n m p in
        Printf.printf "%d\n" (glouton g)
    in
    List.iter traiter [(3, 3, 4); (20, 2, 5); (40, 5, 7)]

(* Question 6 *)

(* Crée la matrice M de voisinage décrite par le sujet *)
let creer_M n =
    let nn = n * n in
    let m = Array.make_matrix nn nn 0 in
    for i = 0 to n - 1 do
        for j = 0 to n - 1 do
            let traiter (i', j') =
                m.(n * i + j).(n * i' + j') <- 1
            in
            List.iter traiter (voisinage i j n)
        done
    done;
    m

(* Crée la matrice colonne correspondant à la grille *)
let creer_VG g =
    let n = Array.length g in
    let vg = Array.make_matrix (n * n) 1 0 in
    for i = 0 to n - 1 do
        for j = 0 to n - 1 do
            vg.(n * i + j) <- [|g.(i).(j)|]
        done
    done;
    vg

(* Opération inverse de la fonction précédente *)
let creer_c vc n = 
    let c = Array.make_matrix n n 0 in
    for i = 0 to n - 1 do
        for j = 0 to n - 1 do
            c.(i).(j) <- vc.(n * i + j).(0)
        done
    done;
    c

(* On crée des fonctions pour les opérations de transposition
   et de transvection, qu'on appliquera à la matrice M et à la 
   matrice colonne VG. Inutile de faire des dilatations, car on
   est dans Z/2Z (et qu'une multiplication par 0 n'est pas
   intéressante) *)
let transposition mat i j =
    let tmp = mat.(i) in
    mat.(i) <- mat.(j);
    mat.(j) <- tmp

let transvection mat i j =
    for k = 0 to Array.length mat.(i) - 1 do
        mat.(i).(k) <- (mat.(i).(k) + mat.(j).(k)) mod 2
    done

(* Dès lors, on applique le pivot de Gauss : on transforme la
   matrice M en identité via des transpositions et transvection, 
   et on applique les mêmes opérations sur VG *)
let pivot_gauss m v =
    let nn = Array.length v in
    try for j = 0 to nn - 1 do
        let i = ref j in
        while m.(!i).(j) = 0 do incr i done;
        transposition m !i j;
        transposition v !i j;
        for i = 0 to nn - 1 do
            if i <> j && m.(i).(j) = 1 then begin
                transvection m i j;
                transvection v i j
            end
        done
    done;
    with _ -> failwith "Pas de solution"

let resoudre g =
    let n = Array.length g in
    let m = creer_M n and vg = creer_VG g in
    try 
        pivot_gauss m vg;
        let c = creer_c vg n in
        somme_controle c
    with _ -> -1

let q6 u0 =
    let traiter (n, m, p) = 
        let g = grille u0 n m p in
        Printf.printf "%d\n" (resoudre g);
        flush stdout
    in
    List.iter traiter [(6, 4, 7); (31, 4, 7); (43, 4, 7); (52, 4, 7)]    