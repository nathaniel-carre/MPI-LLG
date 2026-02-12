(* file correspond au nom du fichier donné en argument de l'interprétation
   ou de l'exécution du fichier. *)
let file = Sys.argv.(1)

(* Permet de lire tout le fichier correspondant à un programme. On lit ligne
   par ligne, et on concatène le tout. *)
let read file =
    let ic = open_in (file ^ ".oklm") in
    let program = ref "" in
    begin try 
        while true do
            program := !program ^ "\n" ^ input_line ic
        done
    with End_of_file -> close_in ic end;
    !program

type token =
    | Init
    | Print
    | While
    | End
    | Plus
    | Minus
    | Equal
    | Var of string
    | Nat of string

(* Permet de lever une exception en indiquant la chaîne problématique. *)
exception LexicalError of string

(* Convertit un lexème sous forme de chaîne en un objet de type token. 
   Pour les variables et les entiers, on vérifie les conditions. *)
let token_of_string = function
    | "init" -> Init
    | "print" -> Print
    | "while" -> While
    | "end" -> End
    | "+" -> Plus
    | "-" -> Minus
    | "=" -> Equal
    | str when str.[0] = 'v' -> 
        let nb = String.sub str 1 (String.length str - 1) in
        (try ignore (int_of_string nb); Var str with _ -> raise (LexicalError str))
    | str -> try ignore (int_of_string str); Nat str with _ -> raise (LexicalError str)

(* On commence par remplacer tous les caractères blancs par des espaces, puis
   on découpe selon le caractère espace, on supprime les chaînes vides (ce qui
   correspond au cas où il y a plusieurs caractères blancs consécutifs) et on
   convertit le reste grâce à la fonction précédente. *)
let lexer str =
    let replace = function
        | '\r' | '\n' | '\t' -> ' '
        | c -> c
    in
    let str = String.map replace str in
    let lst = String.split_on_char ' ' str in
    List.map token_of_string (List.filter ((<>) "") lst)

exception SyntaxError

type prog = 
    | Eps
    | Prog of instr * prog
and instr =
    | I of string
    | Pr of string
    | Pl of string
    | M of string
    | E of string * string
    | W of string * instr * prog

(* On effectue une analyse top-down, avec deux fonctions mutuellement
   récursives. L'indice i permet de savoir où on en est dans la lecture. *)
let parser lst =
    let tab = Array.of_list lst in
    let n = Array.length tab in
    let i = ref 0 in
    let rec prog () =
        if !i = n || tab.(!i) = End then Eps
        else let a = instr () in Prog (a, prog ())
    and instr () = 
        i := !i + 2;
        match tab.(!i - 2), tab.(!i - 1) with
            | Init, Var v -> I v
            | Plus, Var v -> Pl v
            | Minus, Var v -> M v
            | Equal, Var v -> 
                incr i;
                begin match tab.(!i - 1) with
                    | Var w | Nat w -> E (v, w)
                    | _ -> raise SyntaxError
                end
            | Print, Var v -> Pr v
            | While, Var v ->
                let a = instr () in
                let b = prog () in
                if tab.(!i) = End then
                    (incr i; W (v, a, b))
                else raise SyntaxError
            | _ -> raise SyntaxError
    in
    try prog () with _ -> raise SyntaxError

(* On crée une chaîne contenant l'entête du programme, et le début de la
   définition de la fonction main. On utilise une référence tabul pour
   garder en mémoire le nombre de tabulations au début des lignes. C'est le
   rôle de la fonction tabuler que d'ajouter le bon nombre de tabulation.
   Ensuite, on écrit deux fonctions mutuellement récursives, un peu sur le même
   schéma que pour l'analyse syntaxique, dont le rôle est d'écrire un programme ou
   une instruction seule. *)
let transpiler a =
    let c = ref "#include <stdio.h>\n\nint main(void){\n" in
    let tabul = ref 1 in
    let tabulate () = c := !c ^ String.make !tabul '\t' in
    let rec write_prog = function
        | Eps -> 
            decr tabul; 
            if !tabul = 0 then
                c := !c ^ "\treturn 0;\n"
            else
                tabulate (); 
            c := !c ^ "}\n"
        | Prog (a, b) -> write_instr a; write_prog b
    and write_instr a = 
        tabulate ();
        match a with
            | I v -> c := !c ^ "int " ^ v ^ " = 0;\n"
            | Pl v -> c := !c ^ v ^ "++;\n"
            | M v -> c := !c ^ v ^ "--;\n"
            | E (v, w) -> c := !c ^ v ^ " = " ^ w ^ ";\n"
            | Pr v -> c := !c ^ "printf(\"%d\\n\", " ^ v ^ ");\n"
            | W (v, a, b) ->
                c := !c ^ "while (" ^ v ^ " > 0) {\n";
                incr tabul;
                write_instr a;
                write_prog b
    in
    write_prog a;
    !c

(* Pour écrire le code dans un nouveau fichier, on ouvre le fichier et
   on écrit toute la chaîne de caractères. *)
let write c =
    let oc = open_out (file ^ ".c") in
    output_string oc c;
    close_out oc

(* Ce code permet de faire le bon enchaînement de commandes : |> permet d'indiquer
   que ce qui a déjà été calculé est donné en argument à la fonction qui suit. *)
let () =
    file |> read |> lexer |> parser |> transpiler |> write

(* 
prog1 : pas d'erreur, cela correspond au programme décrit dans l'énoncé. Le programme calcule 
        la somme des entiers de 1 à v1.
prog2 : pas d'erreur. Le programme calcule la partie entière supérieure de la racine carrée de v1.
prog3 : Erreur lexicale : il y a écrit 'Init' au lieu de 'init'.
        Erreur syntaxique : il manque les 'end' à la fin des boucles.
        Il y avait une erreur de code non intentionnelle, ce qui fait que le programme n'affiche
        que des zéros (alors que l'objectif était plutôt d'afficher les v1 premiers termes de la
        suite de Fibonacci).
prog4 : pas d'erreur. Le programme affiche les termes de la suite de Syracuse commençant à v1,
        jusqu'à ce que la suite vaille 1.
prog5 : Erreur lexicale : il y a la lettre capitale O au lieu du chiffre 0 lors de certaines
        affectations.
        Le programme affiche 1 ou 0 selon que v1 est premier ou non (mais se trompe sur les
        entiers 0, 1, 2, 3).
*)