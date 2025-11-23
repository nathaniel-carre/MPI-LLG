type puissance4 = {grille : int array array;
                   libres : int array;
                   mutable joueur : int};;

let afficher jeu =
    let open Printf in
    let symb = [|"\x1b[34m\x1b[4m \x1b[0m"; "\x1b[31m\x1b[4m●\x1b[0m"; "\x1b[33m\x1b[4m●\x1b[0m"|] in
    print_string "\x1b[34m _ _ _ _ _ _ _\x1b[0m\n";
    for i = 0 to 5 do
       print_string "\x1b[34m|\x1b[0m";
        for j = 0 to 6 do
            printf "%s\x1b[34m|\x1b[0m" symb.(jeu.grille.(i).(j))
        done;
        print_newline ();
    done;
    print_string "\x1b[34m\x1b[1m 0 1 2 3 4 5 6\x1b[0m\n";;