(* On utilise la division euclidenne pour trouver les coordonnées. *)
let coords s l c =
    s / c, s mod c

(* On crée une liste vide, et on envisage les quatre voisins possibles.
   On utilise les coordonnées pour vérifier s'ils sont bien dans la
   grille. *)
let voisins_grille s l c =
    let i, j = coords s l c in
    let lst = ref [] in
    if i > 0 then lst := s - c :: !lst;
    if j > 0 then lst := s - 1 :: !lst;
    if i < l - 1 then lst := s + c :: !lst;
    if j < c - 1 then lst := s + 1 :: !lst;
    !lst

(* On applique l'algorithme de Prim :
    - on crée une file de priorité dans laquelle on ajoute initialement les
      voisins du sommet 0
    - à chaque étape, on extrait un sommet, on rajoute l'arête si le sommet 
      n'est pas déjà relié au reste du labyrinthe, puis on ajoute ses voisins
      à la file de priorité. *)
let laby_prim l c =
    let n = l * c in
    let g = Array.make n [] in
    let aretes = ref [] in
    let fp = creer_vide () in
    List.iter (fun t -> ajouter fp (Random.int n) (0, t)) (voisins_grille 0 l c);    
    let vus = Array.init n ((=) 0) in
    let k = ref 1 in
    while !k < n do
        let (p, (s,t)) = extraire_min fp in
        if not vus.(t) then begin
            vus.(t) <- true; incr k;
            aretes := (s, t) :: !aretes;
            g.(s) <- t :: g.(s);
            g.(t) <- s :: g.(t);
            List.iter (fun u -> ajouter fp (Random.int n) (t, u)) (voisins_grille t l c);
        end
    done;
    (g, l, c), List.rev !aretes

exception Fini

(* C'est un parcours de graphe. On crée un tableau pour garder en mémoire
   les sommets déjà vus. Pour remplir le tableau des prédécesseurs, la
   fonction auxiliaire a un argument supplémentaire qui est le parent du
   sommet courant. On utilise une levée d'exception pour arrêter les calculs
   lorsqu'on a atteint le sommet final. *)
let reso_prof (g, l, c) =
    let n = l * c in
    let pred = Array.make n (-2) in
    let pref = ref [] in
    let rec dfs parent s =
        if pred.(s) = -2 then begin
            pred.(s) <- parent;
            pref := s :: ! pref;
            if s = n - 1 then raise Fini;
            List.iter (dfs s) (melange g.(s))
        end
    in
    (try dfs (-1) 0; with _ -> ());
    pred, List.rev !pref

(* On écrit une fonction qui calcule l'heuristique. On sait que le sommet
   d'arrivée a les coordonées les plus élevées. *)
let h s l c =
    let is, js = coords s l c and
        it, jt = coords (l * c - 1) l c in
    it - is + jt - js

(* On peut alors écrire l'algorithme A*. À chaque étape, on extrait le sommet
   qui minimise l'heuristique (et pas la somme coût + heuristique). *)
let astar (g, l, c) =
    let n = l * c in
    let pred = Array.make n (-2) in
    let ordre = ref [] in
    let fp = creer_vide () in
    ajouter fp 0 (-1, 0);
    while pred.(n - 1) = -2 do
        let p, (s, t) = extraire_min fp in
        pred.(t) <- s;
        ordre := t :: !ordre;
        List.iter (fun u -> if pred.(u) = -2 then ajouter fp (h u l c) (t, u)) (melange g.(t));
    done;
    pred, List.rev !ordre

(* On fait un parcours en profondeur du graphe grille et on garde en mémoire
   l'arborescence du parcours. Pour avoir de l'aléatoire, on mélange les voisins
   avant de faire les appels récursifs. Comme pour la résolution, la fonction
   auxiliaire a en argument le parent du sommet courant. *)
let laby_prof l c =
    let n = l * c in
    let g = Array.make n [] in
    let aretes = ref [] in
    let vus = Array.make n false in
    let rec dfs s t =
        if not vus.(t) then begin
            vus.(t) <- true;
            if s <> -1 then begin
                aretes := (s, t) :: !aretes;
                g.(s) <- t :: g.(s);
                g.(t) <- s :: g.(t)
            end;
            List.iter (dfs t) (melange (voisins_grille t l c))
        end
    in
    dfs (-1) 0;
    (g, l, c), List.rev !aretes

type 'a randdata = {data : 'a array; mutable taille : int}

(* On se contente d'ajouter l'élément au premier indice disponible. 
   On pense à incrémenter la taille (attention, incr ne fonctionne pas). *)
let push x rd =
    assert (rd.taille < Array.length rd.data);
    rd.data.(rd.taille) <- x;
    rd.taille <- rd.taille + 1

(* Pour retirer un élément, on tire un indice aléatoire, on garde la
   valeur à cet indice en mémoire, puis on y met le dernier élément
   avant de décrémenter la taille et de renvoyer la valeur gardée. *)
let pop rd =
    let i = Random.int rd.taille in
    let x = rd.data.(i) in
    rd.taille <- rd.taille - 1;
    rd.data.(i) <- rd.data.(rd.taille);
    x

let laby_alea l c =
    let n = l * c in
    let g = Array.make n [] in
    let aretes = ref [] in
    let vus = Array.make n false in
    let rd = {data = Array.make n (0, 0); taille = 0} in
    push (-1, 0) rd;
    while rd.taille > 0 do
        let (s, t) = pop rd in
        if not vus.(t) then begin
            vus.(t) <- true;
            if s <> -1 then begin
                aretes := (s, t) :: !aretes;
                g.(s) <- t :: g.(s);
                g.(t) <- s :: g.(t)
            end;
            List.iter (fun u -> push (t, u) rd) (voisins_grille t l c)
        end
    done;
    (g, l, c), List.rev !aretes

let laby_rec l c =
    let n = l * c in
    let g = Array.make n [] in
    let aretes = ref [] in
    let ajout_arete (s, t) =
        aretes := (s, t) :: !aretes;
        g.(s) <- t :: g.(s);
        g.(t) <- s :: g.(t)
    in
    let rec creer_laby s0 ll cc =
        (* Cette fonction auxiliaire crée un labyrinthe récursif, 
           en supposant que le premier sommet est le sommet s0 dans
           le labyrinthe principal. On fait attention à distinguer entre
           le nombre de lignes du sous-graphe et du graphe principal. *)
        if ll = 1 then (* Une seule ligne *)
            for j = 1 to cc - 1 do
                ajout_arete (s0 + (j - 1), s0 + j)
            done
        else if cc = 1 then (* Une seule colonne *)
            for i = 1 to ll - 1 do
                ajout_arete (s0 + (i - 1) * c, s0 + i * c)
            done
        else begin 
        (* Cas général : on fait 4 appels récursifs, et on ajoute 3 arêtes, 
           choisies aléatoirement parmi 4 possibles. On fait attention aux
           calculs des coordonées. *)
            creer_laby s0 (ll / 2) (cc / 2);
            creer_laby (s0 + cc / 2) (ll / 2) (cc - cc / 2);
            creer_laby (s0 + c * (ll / 2)) (ll - ll / 2) (cc / 2);
            creer_laby (s0 + cc / 2 + c * (ll / 2)) (ll - ll / 2) (cc - cc / 2);
            let l1 = Random.int (ll / 2) and
                l2 = ll / 2 + Random.int (ll - ll / 2) and
                c1 = Random.int (cc / 2) and
                c2 = cc / 2 + Random.int (cc - cc / 2) in
            let a1 = (s0 + cc / 2 - 1 + l1 * c, s0 + cc / 2 + l1 * c) and
                a2 = (s0 + cc / 2 - 1 + l2 * c, s0 + cc / 2 + l2 * c) and
                a3 = (s0 + (ll / 2 - 1) * c + c1, s0 + (ll / 2) * c + c1) and
                a4 = (s0 + (ll / 2 - 1) * c + c2, s0 + (ll / 2) * c + c2) in
            List.iter ajout_arete (List.tl (melange [a1; a2; a3; a4]));
        end
    in
    creer_laby 0 l c;
    (g, l, c), List.rev !aretes

(* Suite du corrigé en cours de rédaction. *)

let main =
    Random.self_init ();
    (*
    let laby0 = ([|[3]; [1]; [2;4]; [0;4]; [1;3;5]; [4]|], 2, 3) in
    afficher_laby laby0 20;

    let laby, aretes = laby_prim 20 30 in
    dessiner_laby laby aretes 10 0.001;

    let pred, ordre = reso_prof laby in
    dessiner_resolution laby pred ordre 10 0.001;

    let predastar, ordreastar = astar laby in
    dessiner_resolution laby predastar ordreastar 10 0.001;

    let laby2, aretes2 = laby_prof 20 30 in
    dessiner_laby laby2 aretes2 10 0.001;

    let laby3, aretes3 = laby_alea 20 30 in
    dessiner_laby laby3 aretes3 10 0.001;

    let laby4, aretes4 = laby_rec 20 30 in
    dessiner_laby laby4 aretes4 10 0.01;*)

