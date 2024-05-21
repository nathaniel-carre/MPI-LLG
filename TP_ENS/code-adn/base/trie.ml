(** ============== START PRELUDE ================== **)

(** Utilitaires *)

(** `read_text filename` lit la chaine dans le fichier `filename` et renvoi une string *)
let read_text (file : string) : string =
  let c = open_in file in
  input_line c

(** `read_motif(filename)` lit le motif dans le fichier `filename` et renvoi une liste de string *)
let read_motif (file : string) : string list =
  let c = open_in file in
  let rec aux () =
    try let s = input_line c in s :: aux () with
    | End_of_file -> []
  in
  aux ()

(** Tries *)

type node = Empty | Node of (bool * trie)
and trie = { a:node; c:node; g:node; t:node }

let pp fmt =
  let rec pp_node fmt (final, strie) =
    if final then Format.fprintf fmt "@<1>✓";
    if strie = { a=Empty; c=Empty; g=Empty; t=Empty } then ()
    else Format.fprintf fmt "@[<v>@<1>┐%a@]" pp_tree strie
  and pp_branch fmt c node = match node with
    | Empty -> ()
    | Node node -> Format.fprintf fmt "@,@<1>└%c%a" c pp_node node
  and pp_tree fmt t =
    pp_branch fmt 'A' t.a;
    pp_branch fmt 'C' t.c;
    pp_branch fmt 'G' t.g;
    pp_branch fmt 'T' t.t;
  in
  Format.fprintf fmt "@[<v>%a@]" pp_tree

(** `print_trie t` imprime le trie `t` *)
let print_trie (t: trie) : unit = Format.printf "%a@." pp t

let empty = { a=Empty; c=Empty; g=Empty; t=Empty }
let node f t = Node (f, t)

(** Un trie simple, correspondant au motif A, ACT, CGG, CGAC, TC *)
let simple_trie = {
  a = node true {empty with
                 c = node false {empty with t=node true empty}};
  c = node false {empty with
                  g = node false
                      {empty with
                       g = node true empty;
                       a = node false {empty with c = node true empty}}};
  g = Empty;
  t = node false {empty with
                  c = node true empty};
}

let () =
  print_trie simple_trie

(** ============== STOP PRELUDE ================== **)
