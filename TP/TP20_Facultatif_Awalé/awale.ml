let coeff = function
    | 7  -> 3432
    | 24 -> 2629575
    | _  -> 0

let creer_tabs k u0 n =
    (* Pour k = 7 ou k = 24, calcule les tableaux u, vk et wk *)
    let tab_u = Array.make (n + 1) u0 and
        tab_v = Array.make (n + 1) 0 and
        tab_w = Array.make (n + 1) 0 in
    for i = 1 to n do
        tab_u.(i) <- (1103515245 * tab_u.(i - 1) + 12345) mod (1 lsl 31);
        tab_v.(i) <- (coeff k * tab_u.(i)) / (1 lsl 31);
        tab_w.(i) <- k + 25 * tab_v.(i)
    done;
    tab_u, tab_v, tab_w

let q1 u0 = 
    let f (k, n) =
        let tab_u, tab_v, tab_w = creer_tabs k u0 n in
        Printf.printf "(%d, %d, %d)\n" (tab_u.(n) mod 37)
                                       (tab_v.(n) mod 37)
                                       (tab_w.(n) mod 37); in
    List.iter f [(7,1); (24, 1000); (24, 1_000_000)]

let _ = q1 42

let h = Hashtbl.create 1

let rec binom i j =
    (* Calcule le coefficient binomial i parmi j en mémoïsant *)
    if not (Hashtbl.mem h (i, j)) then begin
        if i > j then Hashtbl.add h (i, j) 0
        else if i = j || i = 0 then Hashtbl.add h (i, j) 1
        else 
            let bij = binom (i - 1) (j - 1) + binom i (j - 1) in
            Hashtbl.add h (i, j) bij
    end;
    Hashtbl.find h (i, j)

let q2 u0 =
    let tab, _, _ = creer_tabs 7 u0 15 in
    let f n =
        let i = 4 + tab.(n) mod 4 in
        let j = i + 5 + tab.(n + 1) mod 20 in
        Printf.printf "%d\n" (binom i j) in
    List.iter f [10; 12; 14]

let _ = q2 42

let enc c =
    (* Calcule l'encodage d'un vecteur. *)
    let s = ref 0 in
    for i = 1 to 7 do
        s := !s + binom i c.(i - 1)
    done;
    !s

let dec g =
    (* Calcule le décodage d'un nombre de Gödel *)
    let c = Array.make 7 0 in
    let gi = ref g in
    for i = 6 downto 0 do
        let ci = ref (-1) in        
        while binom (i + 1) (!ci + 1) <= !gi do incr ci done;
        c.(i) <- !ci;
        gi := !gi - binom (i + 1) !ci
    done;
    c

let afficher_tab t =
    let n = Array.length t in
    Printf.printf "[|%d" t.(0);
    for i = 1 to n - 1 do
        Printf.printf "; %d" t.(i)
    done;
    Printf.printf "|]\n"

let q3 u0 =
    let f (k, n) =
        let _, tab, _ = creer_tabs k u0 n in
        afficher_tab (dec tab.(n)) in
    List.iter f [(7,1); (7,2); (7,3); (24,4); (24,5); (24,6)]

let _ = q3 42

let calcul_D g =
    (* Calcule la configuration associée à un nombre 
       de Gödel *)
    let n = g mod 25 in
    let c = dec (g / 25) in
    let a = Array.make 8 0 in
    a.(0) <- c.(0);
    let s = ref c.(0) in
    for i = 1 to 6 do
        a.(i) <- c.(i) - c.(i - 1) - 1;
        s := !s + a.(i)
    done;
    a.(7) <- n - !s;
    a

let q4 u0 =
    let f (k, n) =
        let _, _, tab = creer_tabs k u0 n in
        afficher_tab (calcul_D tab.(n)) in
    List.iter f [(7,1); (7,2); (7,3); (24,4); (24,5); (24,6)]

let _ = q4 42