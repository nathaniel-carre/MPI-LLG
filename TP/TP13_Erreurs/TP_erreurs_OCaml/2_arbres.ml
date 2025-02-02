type 'a arbre = Vide | Noeud of 'a * 'a arbre * 'a arbre

let rec appartient a x = match a with
  |Vide -> false
  |Noeud(x,g,d) -> true
  |Noeud(_,g,d) -> (appartient g x) || (appartient d x)

let rec nb_differents a val = match a with
  |Vide -> 0
  |Noeud(r,g,d) when r = val -> 1 + nb_differents g val + nb_differents d val
  |Noeud(r,g,d) when r <> val -> nb_differents g val + nb_differents d val

let hauteur a = match a with
  |Vide -> -1
  |Noeud(_,g,d) -> 1 + max (hauteur g) (hauteur d)

(*L'arbre en entrée vérifie que chaque noeud a une étiquette plus petite que celle de ses fils éventuels et on souhaite que cette propriété soit conservée.*)
let rec inserer a x =
  let rec hauteur a = match a with
    |Vide -> 0
    |Noeud(_,_,_) -> 1 + max (hauteur g) (hauteur d)
  in match a with
     |Vide -> Noeud(x, Vide, Vide)
     |Noeud(r,g,d) -> let hg = hauteur g and hd = hauteur d in
                      if hd <= hg then min(r, x), g, inserer_1 d max(r, x)
                      else min(r, x), inserer_1 max(r, x), d
