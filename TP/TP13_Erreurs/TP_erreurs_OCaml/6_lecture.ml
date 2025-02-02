let lire_lignes (fichier:string) : string list =
  let lignes = ref [] in
  let flux = open_in fichier in
  try
    while true do
      lignes := input_line flux::(!lignes)
    done
  with End_of_file -> List.rev !lignes

let ll = lire_lignes "toto.txt"
