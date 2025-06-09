#require "topfind";;
#use "PQ.ml";;

open Scanf;;
(* open PQ;; *)


(* La fonction lit_graphe renvoie un tableau de liste de paires
  représentant le graphe en liste d'ajacence c'est à dire que si x est
  voisin de y à distance d alors la fonction renverra un tableau t tel
  que la liste t.(x) contiendra la paire (y,d) et t.(y) contiendra la
  paire (x,d) *)   
let lit_graphe () = 
  let fp_graphe = Scanning.open_in "graphe.txt" in
  let read_int () = bscanf fp_graphe "%d" (fun x:int -> x) in
  let read_arete () = bscanf fp_graphe " %d %f" (fun x y -> (x,y)) in
  let read_newline () = bscanf fp_graphe "%c" (fun x -> ()) in
  let nb_noeuds = read_int () in
  let _ = read_newline () in
  let graphe = Array.make nb_noeuds [] in
  for i = 0 to nb_noeuds-1 do
    let nb_voisins = read_int () in
    for j = 0 to nb_voisins-1 do
      graphe.(i) <- read_arete () :: graphe.(i)
    done ;
    read_newline () 
  done ;
  Scanning.close_in fp_graphe ;
  graphe;;

let lit_position () =
  let fp_pos = Scanning.open_in "positions.txt" in
  let read_int () = bscanf fp_pos "%d%c" (fun x _ -> x) in
  let read_paire x = bscanf fp_pos " %f %f%c" (fun x y _-> (x,y)) in
  let nb_noeuds = read_int () in
  let positions = Array.init nb_noeuds read_paire in
  Scanning.close_in fp_pos ;
  positions;;  
  
let graphe = lit_graphe ();;
let positions = lit_position ();;

let nombre_composantes g =
    let n = Array.length g in
    let vus = Array.make n false in
    let nb_cc = ref 0 in
    let pile = Stack.create () in
    for s = 0 to n - 1 do
        if not vus.(s) then begin
            incr nb_cc;
            Stack.push (s, 0.) pile;
            while not (Stack.is_empty pile) do
                let (s, _) = Stack.pop pile in
                if not vus.(s) then begin
                    vus.(s) <- true;
                    List.iter (fun x -> Stack.push x pile) g.(s)
                end
            done;
        end
    done;  
    !nb_cc;;

exception Trouve;;

let dijkstra g s1 s2 =
    let n = Array.length g in
    let dist = Array.make n infinity and
        pred = Array.make n (-1) in
    dist.(s1) <- 0.0; pred.(s1) <- s1;
    let fp = ref file_vide in
    let ajout s = fp := ajoute !fp s dist.(s) in
    let extraire () = let x = recupere_min !fp in fp := retire_min !fp; x in
    ajout s1;
    let explores = ref 0 in
    begin try while !fp <> file_vide do
            let (s, ds) = extraire () in
            incr explores;
            if s = s2 then raise Trouve;
            let traiter (t, st) =
                if ds +. st < dist.(t) then begin
                    dist.(t) <- ds +. st;
                    pred.(t) <- s;
                    ajout t;
                end in
            List.iter traiter g.(s)
        done;
    with Trouve -> () end;
    let chemin = ref [s2] in
    while pred.(List.hd !chemin) <> s1 do
        chemin := pred.(List.hd !chemin) :: !chemin;
    done;
    !chemin, dist.(s2), !explores;;

let haversine s1 s2 =
    let x1, y1 = positions.(s1) and
        x2, y2 = positions.(s2) in
    let ratio = Float.pi /. 180. in
    let v1 = sin((y1-.y2) *. ratio /. 2.) ** 2. and
        v2 = sin((x1-.x2) *. ratio /. 2.) ** 2. in
    2. *. 6371000. *. asin(sqrt(v1 +. cos(y1 *. ratio) *. cos(y2 *. ratio) *. v2));;

let astar g s1 s2 =
    let n = Array.length g in
    let dist = Array.make n infinity and
        pred = Array.make n (-1) in
    let fp = ref file_vide in
    let h s = haversine s s2 in
    let ajout s = fp := ajoute !fp s (dist.(s) +. h s) in
    let extraire () = let x = recupere_min !fp in fp := retire_min !fp; x in
    dist.(s1) <- 0.0; pred.(s1) <- s1; ajout s1;
    let explores = ref 0 in
    begin try while !fp <> file_vide do
            let (s, ds) = extraire () in
            incr explores;
            if s = s2 then raise Trouve;
            let traiter (t, st) =
                if dist.(s) +. st < dist.(t) then begin
                    dist.(t) <- dist.(s) +. st;
                    pred.(t) <- s;
                    ajout t;
                end in
            List.iter traiter g.(s)
        done;
    with Trouve -> () end;
    let chemin = ref [s2] in
    while pred.(List.hd !chemin) <> s1 do
        chemin := pred.(List.hd !chemin) :: !chemin;
    done;
    !chemin, dist.(s2), !explores;;