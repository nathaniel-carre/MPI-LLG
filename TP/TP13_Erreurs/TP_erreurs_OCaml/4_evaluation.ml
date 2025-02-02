(*Renvoie la liste [n; n-1; ...; 1; 0]*)
let rec decompte n =
  let m = n+1 in
  if m = 0 then []
  else (m-1) :: decompte (m - 1);;

let _ = decompte 8

(*Calcule le tableau contenant k/2^i en case i pour chaque i entre 0 et n*)
(*quotients 3 7 devrait renvoyer [|7;3;1|]*)
let quotients (n:int) (k:int) =
  let q = Array.make n k in
  Array.iter (fun i -> if (i=0) then () else q.(i) <- (q.(i-1))/2) q

let _ = quotients 3 7
