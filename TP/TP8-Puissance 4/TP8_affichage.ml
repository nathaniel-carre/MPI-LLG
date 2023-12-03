type puissance4 = {grille : int array array;
                   libres : int array;
                   mutable joueur : int;
                   mutable vides : int};;

let afficher jeu =
    let open Printf in
    let symb = [|"_"; "X̲"; "O̲"|] in
    print_string " _ _ _ _ _ _ _\n";
    for i = 0 to 5 do
        print_string "|";
        for j = 0 to 6 do
            printf "%s|" symb.(jeu.grille.(i).(j))
        done;
        print_newline ();
    done;
    print_string " 0 1 2 3 4 5 6\n";;   