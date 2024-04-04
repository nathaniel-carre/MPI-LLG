(* Question 1 *)

(* Un classique, pas de difficulté ici. *)
let swap tab i j =
    let tmp = tab.(i) in
    tab.(i) <- tab.(j);
    tab.(j) <- tmp;;

(* Question 2 *)

(* On commence par choisir un pivot aléatoire, qu'on met au début
   de la partition. Ensuite, on parcourt tous les éléments, en gardant
   comme invariant de boucle que les éléments entre lo + 1 (inclus) et 
   i (inclus) sont ont l'attribut a inférieur à celui du pivot, et ceux
   entre i + 1 (inclus) et j (exclu) ont cet attribut supérieur à celui
   du pivot. On termine en mettant le pivot à la bonne place. *)
let partition data lo hi a =
    swap data lo (lo + Random.int (hi - lo));
    let i = ref lo in
    for j = lo + 1 to hi - 1 do
        if data.(j).(a) <= data.(lo).(a) then begin
            incr i;
            swap data !i j
        end
    done;
    swap data lo !i;
    !i;;

type tree = Nil | Node of tree * int * tree;;

(* Question 3 *)

(* On se contente de partitionner, et de relancer un appel récursif sur chaque
   côté pour créer l'arbre final. *)

let rec kd_tree_aux data lo hi a =
    if lo = hi then Nil else begin
        let k = Array.length data.(lo) in
        let i = partition data lo hi a in
        let left = kd_tree_aux data lo i ((a + 1) mod k) and
            right = kd_tree_aux data (i + 1) hi ((a + 1) mod k) in
        Node(left, i, right)
    end;;

(* Question 4 *)

(* La fonction est directe en utilisant la précédente, en créant une copie des 
   données. Pas besoin de copie profonde ici, car il n'est pas prévu de modifier
   les données, juste leurs positions relatives. *)

let kd_tree data = 
    let data = Array.copy data in
    data, kd_tree_aux data 0 (Array.length data) 0;;

(* Question 5 et 6 *)

(* Pas de difficulté ici. *)

let create_point k =
    Array.init k (fun _ -> Random.float 100.);;

let create_data n k =
    Array.init n (fun _ -> create_point k);;

(* Question 7 *)

(* Un classique. La hauteur de l'arbre vide est -1. *)

let rec height = function
    | Nil -> -1
    | Node(l, _, r) -> 1 + max (height l) (height r);;

(* Question 8 *)

(* Pour garantir la complexité linéaire, on passe par une fonction auxiliaire. *)

let infix tree =
    let rec infix_aux tree acc = match tree with
        | Nil -> acc
        | Node(l, i, r) -> infix_aux l (i :: infix_aux r acc) in
    infix_aux tree [];;

(* Question 9 *)

(* On peut vérifier « à la main » le résultat, ou écrire une fonction qui vérifie
   qu'une liste est croissante. *)

let rec increasing = function
	| [] | [_] -> true
	| x :: y :: q -> x <= y && increasing (y :: q);;

let d, t = 
    let data = create_data 100_000 3 in
    kd_tree data;;

assert (increasing (infix t));;
Printf.printf "La hauteur est %d\n" (height t);;

(* Question 10 *)

let delta x y =
    let d = ref 0. in
    for a = 0 to Array.length x - 1 do
        d := !d +. (x.(a) -. y.(a)) ** 2.
    done;
    sqrt !d;;

(* Question 11 *)

(* On applique l'algorithme du cours. Pour éviter d'écrire plusieurs fois
   la même chose selon qu'on explore d'abord l'enfant gauche ou le droit,
   on nomme t1 et t2 le premier et le deuxième arbres explorés respectivement. *)

let nearest_neighbor data tree x =
    let k = Array.length x in
    let imin = ref (-1) and dmin = ref infinity in
    let explored = ref 0 in
    let rec explore a = function
        | Nil -> ()
        | Node(l, i, r) ->
            let d = delta x data.(i) in
            incr explored;
            if d < !dmin then (imin := i; dmin := d);
            let t1, t2 = if x.(a) <= data.(i).(a) then l, r
                         else r, l in
            explore ((a + 1) mod k) t1;
            if !dmin > abs_float (x.(a) -. data.(i).(a)) then
                explore ((a + 1) mod k) t2 in
    explore 0 tree;
    !imin, !explored;;

(* Question 12 *)

let nb_explored n k =
    let data = create_data n k in
    let d, t = kd_tree data in
    let x = create_point k in
    snd (nearest_neighbor d t x);;

let test n k p =
	let sum = ref 0 in
	for _ = 0 to p - 1 do
		sum := !sum + nb_explored n k
	done;
	float !sum /. float p;;

let f k =
	Printf.printf "Pour k = %d, il y a en moyenne %f nœuds explorés.\n"
	    k (test 100_000 k 10) in
List.iter f [3; 10; 20];;

(* Question 13 *)

(* Utilise ici l'algorithme de sélection rapide en reprenant l'algorithme
   de partition. C'est dommage de trier le tableau pour trouver la médiane ! *)

let rec median_aux data lo hi med a =
    let i = partition data lo hi a in
    if i = med then med
    else if i > med then median_aux data lo i med a
    else median_aux data (i + 1) hi med a;;

let median data lo hi a =
    median_aux data lo hi ((lo + hi) / 2) a;;

(* La construction de l'arbre se fait alors sur le même principe, mais en
   utilisant la médiane comme pivot. *)

let rec kd_tree_median_aux data lo hi a =
    if lo = hi then Nil else begin
        let k = Array.length data.(lo) in        
        let i = median data lo hi a in        
        let left = kd_tree_median_aux data lo i ((a + 1) mod k) and
            right = kd_tree_median_aux data (i + 1) hi ((a + 1) mod k) in
        Node(left, i, right)
    end;;

let kd_tree_median data = 
	let data = Array.copy data in
    data, kd_tree_median_aux data 0 (Array.length data) 0;;

(* Question 14 *)

let nb_explored2 n k =
    let data = create_data n k in
    let d1, t1 = kd_tree data and
        d2, t2 = kd_tree_median data in
    let x = create_point k in
    snd (nearest_neighbor d1 t1 x), snd (nearest_neighbor d2 t2 x);;

let test2 n k p =
	let sum1 = ref 0 and sum2 = ref 0 in
	for _ = 0 to p - 1 do
		let e1, e2 = nb_explored2 n k in
		sum1 := !sum1 + e1;
		sum2 := !sum2 + e2
	done;
	float !sum1 /. float p, float !sum2 /. float p;;

let f k =
	let x1, x2 = test2 100_000 k 10 in
	Printf.printf "Pour k = %d :
	 - il y a en moyenne %f nœuds explorés dans le premier arbre.
	 - il y a en moyenne %f nœuds explorés dans le deuxième arbre.\n" k x1 x2; in
List.iter f [3; 10; 20];;

(* Il n'y a pas de différence significative. *)





















