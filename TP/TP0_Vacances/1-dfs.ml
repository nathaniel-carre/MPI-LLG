type graphe = int list array

let dfs s0 g =
    (* On fait un dfs récursif, en rajoutant les éléments dans 
       une référence de liste au fur et à mesure. On pense à 
       renverser la liste avant de la renvoyer. *)
    let n = Array.length g in
    let vus = Array.make n false in
    let lst = ref [] in
    let rec dfs_rec s = 
        if not vus.(s) then begin
            vus.(s) <- true;
            lst := s :: !lst;
            List.iter dfs_rec g.(s)
        end
    in
    dfs_rec s0;
    List.rev !lst

(* Cas orienté *)
let g0 = [|
    [1];
    [0];
    [3; 5];
    [4];
    [0; 1; 6];
    [4];
    []
|]

let _ = assert (dfs 3 g0 = [3; 4; 0; 1; 6])

(* Cas non orienté *)
let g1 = [|
    [4; 6];
    [4];
    [3];
    [2];
    [0; 1; 5; 6];
    [4];
    [0; 4]
|]

let _ = assert (dfs 3 g0 = [1; 4; 0; 6; 5])