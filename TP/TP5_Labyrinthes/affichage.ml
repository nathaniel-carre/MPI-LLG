#require "graphics"
#require "unix"

type laby = int list array * int * int

let afficher_laby (g, l, c) k =
    let coord s l c =
	    2 * l - 2 * (s / c) - 1, 2 * (s mod c) + 1
    in
    let open Graphics in
    let n = 2 * l + 1 and p = 2 * c + 1 in
    let lar = (p + 2) * k and hau = (n + 2) * k in
    let res = " " ^ string_of_int lar ^ "x" ^ string_of_int hau in
    let case i j =
        let i' = (i + 1) * k and j' = (j + 1) * k in
        fill_rect j' i' (k - 1) (k - 1)
    in
    try
    	open_graph res;
    	for i = 0 to n - 1 do
        	for j = 0 to p - 1 do            	
            	if i mod 2 = 1 && j mod 2 = 1 then set_color white
                else set_color black;
                case i j            	
        	done;
    	done;
    	set_color white;
    	for s = 0 to l * c - 1 do
    		let is, js = coord s l c in
    		let afficher_arete t =
    			if s < t then begin
    				let it, jt = coord t l c in
    				let i = (is + it) / 2 and j = (js + jt) / 2 in
    				case i j
    			end
    		in
    		List.iter afficher_arete g.(s)
    	done;
        ignore (read_key ());
        close_graph ();
   	with _ -> close_graph ()

let minisleep (sec: float) =
    Unix.sleepf sec

let dessiner_laby (g, l, c) aretes k dt =
    let coord s l c = 
        2 * l - 2 * (s / c) - 1, 2 * (s mod c) + 1
    in
    let open Graphics in
        let n = 2 * l + 1 and p = 2 * c + 1 in
        let lar = (p + 2) * k and hau = (n + 2) * k in
        let res = " " ^ string_of_int lar ^ "x" ^ string_of_int hau in
        let case i j =
            let i' = (i + 1) * k and j' = (j + 1) * k in
            fill_rect j' i' (k - 1) (k - 1)
        in
        try
            open_graph res;
            set_color black;
            for i = 0 to n - 1 do
                for j = 0 to p - 1 do
                    case i j
                done;
            done;
            minisleep 3.;
            set_color white;
            let afficher_arete (s, t) =
                let is, js = coord s l c and
                    it, jt = coord t l c in
                let i = (is + it) / 2 and j = (js + jt) / 2 in
                case is js; case i j; case it jt; minisleep dt                
    		in
            List.iter afficher_arete aretes;  
            ignore (read_key ());
            close_graph ();
        with _ -> close_graph ()

let dessiner_resolution (g, l, c) pred ordre k dt =
    let coord s l c = 
        2 * l - 2 * (s / c) - 1, 2 * (s mod c) + 1
    in
    let open Graphics in
        let n = 2 * l + 1 and p = 2 * c + 1 in
        let lar = (p + 2) * k and hau = (n + 2) * k in
        let res = " " ^ string_of_int lar ^ "x" ^ string_of_int hau in
        let case i j =
            let i' = (i + 1) * k and j' = (j + 1) * k in
            fill_rect j' i' (k - 1) (k - 1)
        in
        try
            open_graph res;
            for i = 0 to n - 1 do
                for j = 0 to p - 1 do            	
                    if i mod 2 = 1 && j mod 2 = 1 then set_color white
                    else set_color black;
                    case i j            	
                done;
    	    done;
            set_color white;
            for s = 0 to l * c - 1 do
                let is, js = coord s l c in
                let afficher_arete t =
                    if s < t then begin
                        let it, jt = coord t l c in
                        let i = (is + it) / 2 and j = (js + jt) / 2 in
                        case i j
                    end
                in
    		    List.iter afficher_arete g.(s)
            done;
            minisleep 3.;
            set_color yellow;
            let afficher t =
                let s = pred.(t) in
                let is, js = coord s l c and
                    it, jt = coord t l c in
                let i = (is + it) / 2 and j = (js + jt) / 2 in
                if s <> -1 then case i j;
                case it jt; minisleep dt;
    		in
            List.iter afficher ordre;
            set_color green;
            let t = ref (l * c - 1) in
            let it, jt = coord !t l c in
            case it jt;
            while !t <> 0 do
                let s = pred.(!t) in
                let is, js = coord s l c and
                    it, jt = coord !t l c in
                let i = (is + it) / 2 and j = (js + jt) / 2 in
                case i j; case is js; minisleep dt;
                t := s;
            done;
            ignore (read_key ());
            close_graph ();
        with _ -> ()

module FilePrio = Map.Make(Int)
type 'a fileprio = ('a list) FilePrio.t ref

let creer_vide () : 'a fileprio = ref FilePrio.empty

let est_vide (fp: 'a fileprio) = !fp = FilePrio.empty

let ajouter (fp: 'a fileprio) prio valeur =
    fp := FilePrio.update prio 
        (function None -> Some [valeur] 
                | Some lst -> Some (valeur :: lst)) !fp

let extraire_min (fp: 'a fileprio) =
    if est_vide fp then failwith "La file de priorité est vide";
    let prio, lst = FilePrio.min_binding !fp in
    fp := FilePrio.remove prio !fp;
    match lst with
        | [] -> assert false
        | [valeur] -> prio, valeur
        | valeur :: q -> fp := FilePrio.add prio q !fp; prio, valeur

let melange lst =
    let tab = Array.of_list lst in
    for i = Array.length tab to 1 do
        let j = Random.int i in
        let tmp = tab.(j) in
        tab.(j) <- tab.(i - 1);
        tab.(i - 1) <- tmp
    done;
    Array.to_list tab 