type formule =
    | Bot | Top
    | Var of int
    | Neg of formule
    | Et of formule * formule
    | Ou of formule * formule

type valuation = bool array

let rec eval mu = function
    (* Interprétation récursive d'une formule par une valuation. *)
    | Bot -> false
    | Top -> true
    | Var i -> mu.(i)
    | Neg phi -> not (eval mu phi)
    | Et (phi, psi) -> eval mu phi && eval mu psi
    | Ou (phi, psi) -> eval mu phi || eval mu psi

let sat phi n = 
    (* On crée une valuation, puis on écrit une fonction récursive de
       retour sur trace. Si on a remplit la valuation, on interprète
       la formule. Sinon, on met un false à l'indice i et on recommence
       à l'indice i + 1, et pareil avec un true. *)
    let mu = Array.make n false in
    let rec backtrack i = 
        if i = n then eval mu phi
        else
            (mu.(i) <- false; backtrack (i + 1)) ||
            (mu.(i) <- true; backtrack (i + 1))
    in
    backtrack 0

(* Formule satisfiable *)
let phi0 = Ou(Neg (Var 2), Et (Var 0, Ou (Var 1, Neg (Var 3))))

let _ = assert (sat phi0 4)

(* Formule non satisfiable *)
let phi1 = Et (Var 0, 
           Et (Ou (Neg (Var 0), Var 1), 
           Et (Ou (Neg (Var 1), Var 2), 
           Et (Ou (Neg (Var 2), Var 3), 
               Ou (Neg (Var 3), Neg (Var 0))))))

let _ = assert (not (sat phi1 4))