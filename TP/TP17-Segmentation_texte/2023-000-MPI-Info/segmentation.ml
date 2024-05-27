
(********************************************************************)
(* Concours Centrale-Supélec                                        *)
(* Sujet 0 - MPI                                                    *)
(* https://www.concours-centrale-supelec.fr                         *)
(* CC BY-NC-SA 3.0                                                  *)
(********************************************************************)

let texte_preface = "préfacedanslaquelleilestétablique,malgréleursnomsenosetenis,leshérosdel'histoirequenousallonsavoirl'honneurderaconteranoslecteursn'ontriendemythologique."

type lexique = (string, int) Hashtbl.t

(* Lexique correspondant à l'exemple proposé dans le sujet *)
let dico_ex : lexique =
  let h = Hashtbl.create 4 in
  let add (m, c) = Hashtbl.add h m c in
  List.iter add [("a", 1); ("ab", 1); ("aba", 2); ("bb", 1)];
  h

(* Vérifie qu'un fichier de lexique est conforme au format demandé *)
let verifie_format nom_fichier =
  let f = open_in nom_fichier in
  begin
    try
      while true do
        match String.split_on_char ' ' (input_line f) with
        | [mot; occ] ->
           assert (String.length mot <= 100);
           assert (int_of_string occ >= 1);
        | _ -> failwith "Erreur de format"
      done
    with End_of_file -> close_in f
  end
