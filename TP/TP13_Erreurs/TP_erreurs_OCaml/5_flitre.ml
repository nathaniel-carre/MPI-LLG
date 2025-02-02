let rec filtrer seuil l =
  if l = [] then Printf.printf "Fini"; else if List.hd l >= seuil then (List.hd l)::(filtrer seuil List.tl l)
  else filtrer seuil List.tl l

filtrer 5 [1;4;5;1;2;5;4;21;2;54;1;2;5;6]
