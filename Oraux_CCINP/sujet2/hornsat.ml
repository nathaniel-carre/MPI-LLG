(*Types décrit par l'énoncé*)
type clause_horn = int option * int list
type formule_horn = clause_horn list

let f1 = [(None, [0;1;3]); (Some 0, [1]); (Some 0, [2;3]); (Some 2, [0;3]); (Some 2, []); (None, [2;3])]
let f3 = [(None, [1;4]); (Some 1, []); (None, [0;3;4]); (Some 0, [1]); (Some 2, [3;4]); (Some 4, [0;1])]
