type graphe = int list array

exception Cycle

let acyclique g =
    (* On utilise un tableau des sommets vus : 0 = pas vu, 1 = vu et 
       en cours d'exploration, 2 = vu et exploration terminée.
       Le graphe contient un cycle si on renconte un sommet étiqueté
       par 1. 
       Une fois la fonction récursive écrite, on s'assure de l'appeler
       au moins une fois par sommet du graphe. *)
    let n = Array.length g in    
    let vus = Array.make n 0 in
    let rec dfs s =
        if vus.(s) = 1 then raise Cycle;
        if vus.(s) = 0 then begin
            vus.(s) <- 1;
            List.iter dfs g.(s);
            vus.(s) <- 2
        end
    in
    try for s = 0 to n - 1 do
            if vus.(s) = 0 then dfs s
        done;
        true
    with Cycle -> false

(* Cas cyclique *)
let g0 = [|
    [1];
    [0];
    [3; 5];
    [4];
    [0; 1; 6];
    [4];
    []
|]

let _ = assert (not (acyclique g0))

(* Cas acyclique *)
let g1 = [|
    [1];
    [];
    [3; 5];
    [4];
    [0; 1; 6];
    [4];
    []
|]

let _ = assert (acyclique g1)