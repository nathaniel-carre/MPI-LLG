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

(** Machines à motif *)

type charADN = A | C | G | T
type state = int
type machine = {
  init: state;
  transition: state -> charADN -> state option;
  fail: state -> state;
  output: state -> string list;
}

let pp fmt machine =
  let seen = ref [] in
  let pp_output =
    let pp_sep fmt () = Format.fprintf fmt ",@ " in
    Format.pp_print_list ~pp_sep Format.pp_print_string
  in
  let rec pp_state fmt state =
    if List.mem state !seen then ()
    else begin
      seen := state :: !seen;
      let nexts = ref [] in
      Format.fprintf fmt "%i@[<v>@<1>┐output:@[<h>{%a}@]"
        state
        pp_output (machine.output state);
      List.iter (fun (c, cx) ->
          match machine.transition state c with
          | None -> ()
          | Some st ->
            nexts := st :: !nexts;
            Format.fprintf fmt "@,@<1>├%c@<1>→ %i" cx st
        ) [A,'A';C,'C';G,'G';T,'T'];
      Format.fprintf fmt "@,@<1>└fail@<1>→ %i@]@," (machine.fail state);
      List.iter (pp_state fmt) !nexts;
      ()
    end
  in
  Format.fprintf fmt "@[<v>%a@]" pp_state machine.init

(** `print_machine m` imprime la machine `m` *)
let print_machine (m:machine) : unit = Format.printf "%a@." pp m

(** Un machine simple, correspondant au motif A, ACT, CGG, CGAC, TC *)
let simple_machine =
  let init = 0
  and transition st c = match st, c with
    | 0, A -> Some 1
    | 0, G -> Some 2
    | 0, _ -> Some 0
    | 1, A -> Some 3
    | 2, T -> Some 4
    | _ -> None
  and fail = function
    | 0 -> 0 | 1 -> 0 | 2 -> 0 | 3 -> 2 | 4 -> 0
    | _ -> assert false
  and output = function
    | 3 -> ["foo"]
    | 4 -> ["bar"; "baz"]
    | _ -> []
  in
  { init = init ; transition = transition ; fail = fail ; output = output }

let () =
  print_machine simple_machine

(** ============== STOP PRELUDE ================== **)
