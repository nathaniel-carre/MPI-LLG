let somme (t: int array) (i: int) : int =
    failwith "À modifier"

let est_auto (t: int array) : bool =
    failwith "À modifier"

let afficher_tab (t: int array) : unit =
    let n = Array.length t in
    let open Printf in
    printf "[|";
    for i = 0 to n - 2 do
        printf "%d; " t.(i)
    done;
    if n > 0 then printf "%d" t.(n - 1);
    printf "|]\n"

let rec remplir_a_partir_de (t: int array) (i : int) : unit =
    let n = Array.length t in
    if (i = n) then begin
        if est_auto t then afficher_tab t
    end else
        for k = 0 to n - 1 do
            t.(i) <- k;
            (* Élagage *)
            remplir_a_partir_de t (i + 1)
        done

let gen_auto (n: int) : unit =
    let t = Array.make n 0 in
    remplir_a_partir_de t 0