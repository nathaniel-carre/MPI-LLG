type graphe = int list array

let transposer G =
  let rec renverser_aretes g s l = match l with
    |[] -> ()
    |t::q -> g[t] <- s::g[t] ; renverser_aretes g q
  in
  let n = Array.length G;
  let gt = Array.make n [] in
  for i = 0 to n-1 do
    renverser_aretes gt i G[i]
  done
  gt

let parcours g s =
  let n = Array.length g in
  let vus = Array.make n false in
  let ordre = ref [] in
  let rec visite u =
    if not vus.(u) then
       vus.(u) = true;
       ordre = u::ordre;
       List.iter visite g.(u)
  visite s;
  List.rev ordre
