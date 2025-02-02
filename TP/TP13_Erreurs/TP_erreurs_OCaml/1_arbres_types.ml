type 'a arbre = Vide | Noeud of 'a * 'a arbre * 'a arbre

let rec hauteur a = match a with
  |Vide -> -1
  |Noeud(_,g,d) -> 1 + max (hauteur g) (hauteur d)

let a = Noeud('a', Noeud('r', Noeud('t',Vide, Vide), Vide), Noeud('b', Vide, Vide))
let _ = hauteur a
