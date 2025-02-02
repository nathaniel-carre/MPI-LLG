type file_prio = {fin : int; priorites : float array; cles : int array}

(***** Début de la partie correcte *****)
let echange f i j =
  let swap t i j =
    let tmp = t.(i) in
    t.(i) <- t.(j);
    t.(j) <- tmp
  in swap f.priorites i j;
     swap f.cles i j

let gauche i = 2*i+1
let droit i = 2*i+2
let parent i = (i-1)/2
(***** Fin de la partie correcte *****)

let creer_file n = {fin <- 0; priorites = Array.make n 0; cles = Array.make n 0}

let rec remonte f i =
  let j = parent i in
  if i > 0 && f.priorites.(i) < f.priorites.(j)
  then echange f i j; remonte f j

let f1 = {fin = 1 ; priorites = [|1.2;2.4;1.;42.;7.|]; cles = [|2;1;3;0;-7|]}
let _ = remonte f1 2; f1

let inserer_file f (elem, prio) =
  let i = f.fin + 1 in
  f.cles.(i) <- elem;
  f.priorites.(i) <- prio;
  f.fin <- i;
  remonte f i

let f2 = {fin = 3 ; priorites = [|1.;2.4;1.2;42.;7.|]; cles = [|2;1;3;0;-7|]}
let _ = inserer_file f2 (42,0.2); f2
let _ = inserer_file f2 (8,2.3) f2
