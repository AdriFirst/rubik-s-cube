
(* Nous avons un cube composé de sides**3 cubies, sides étant le nombre de cubies composant le côté du cube.
 * Ces cubies ont un certain nombre de certaines couleurs, représentés par le type colors.
 * colors = [|None ; None ; None|] -> le cubie est un centre (invisible de l'exterieur)
 * colors = [|Red ; None ; None |] -> faces (sur une face mais non en bordure)
 * colors = [|Red ; Blue ; None |] -> Vertices (arete)
 * colors = [|Red ; Blue ; White ] -> Edge (coin) *)

(* À noter que color Option représente la dimension (x, y ou z) sur laquelle le cubie est visible. *)
(* Par conséquent, lors d'une rotation du cube, la répartition de ces color Option changent. *)

(* Nous considérons aussi le repère (O, x, y, z) *)

type color = White | Red | Blue | Orange | Green | Yellow
type colors = color option array
type cube = colors array array array

let rotation90 (x0:int) (y0:int) : int*int =
  (* on suppose sides = 3 cad (sides-1)/2 = 1 *)
  if (x0 = 0 || x0 = 2) && (y0 = 0 || y0 = 2) then
    (* c'est un coin *)
    match x0,y0 with
    (* on n'est pas très fort pour trouver des prédicats, nous préfèrerons réaliser une disjonction de cas qui, tristement, ne fonctionnera que pour notre cube 3x3 *)
    | 0, 0 -> 0, 2
    | 0, 2 -> 2, 2
    | 2, 2 -> 0, 2
    | 2, 0 -> 0, 0 | _ -> x0, y0
  else
    (* c'est une arete *)
    match x0, y0 with
    | 0,1 -> 1, 2
    | 1, 2 -> 2, 1
    | 2, 1 -> 1, 0
    | 1, 0 -> 0, 1 | _ -> print_endline "meme coos" ; x0, y0

let rotationmoin90 (x0:int) (y0:int) : int*int =
  (* on copie / colle salement ... *)
  if (x0 = 0 || x0 = 2) && (y0 = 0 || y0 = 2) then
    (* c'est un coin *)
    match x0,y0 with
    (* on n'est pas très fort pour trouver des prédicats, nous préfèrerons réaliser une disjonction de cas qui, tristement, ne fonctionnera que pour notre cube 3x3 *)
    | 0, 0 -> 2, 0
    | 2, 0 -> 2, 2
    | 2, 2 -> 0, 2
    | 0, 2 -> 0, 0 | _ -> x0, y0
  else
    (* c'est une arete *)
    match x0, y0 with
    | 0, 1 -> 1, 0
    | 1, 0 -> 2, 1
    | 2, 1 -> 1, 2
    | 1, 2 -> 0, 1 | _ -> print_endline "meme coos" ; x0, y0 (* c'est un cubie-face *)

let create_cube (sides : int) : cube =
  (* Renvoie un cube initialisé comme complet / réussi. On suppose sides > 1 *)
  let cube = Array.make sides [||] in for i = 0 to sides-1 do cube.(i) <- Array.make_matrix sides sides ([|None ; None ; None|]) done;
  for z = 0 to sides-1 do
    for y = 0 to sides-1 do
      for x = 0 to sides-1 do
        cube.(x).(y).(z) <- [|if x = 0 then Some Red else if x = sides-1 then Some Orange else None;
                              if y = 0 then Some White else if y = sides-1 then Some Yellow else None;
                              if z = 0 then Some Blue else if z = sides-1 then Some Green else None|]
      done;
    done;
  done;
  cube


let get_color (c:color) : string =
  match c with
  | White -> "White"
  | Red -> "Red"
  | Orange -> "Orange"
  | Yellow -> "Yellow"
  | Blue -> "Blue"
  | Green -> "Green"

let char_color_of_cubie (cubie:colors) (dimension:int): char =
  (*renvoie le caractère correspondant à une case-couleur *)
  (* dimension est l'axe sur lequel est étendu la case couleur  *)
  match Option.get cubie.(dimension) with
  | Red -> 'R'
  | Orange -> 'O'
  | White -> 'W'
  | Yellow -> 'Y'
  | Blue -> 'B'
  | Green -> 'G'

let print_cube_pattern_x3 (c:cube) : unit =
  (* à partir d'un cube en 3d, on essaie de déduire un "patron" permettant de représenter les faces telles qu'on pourrait chacunes les voirs ... de face *)

  (* l'affichage est par-rapport à la face orange, il faut donc procèder à des inversions pour que cela nous paraisse correcte par rapport à nous *)
  (* RED - symétrie axiale d'axe colinéaire à y passant par le centre *)
  (* ORANGE - ok! *)
  (* WHITE - rotation de -90degres *)
  (* YELLOW - symétrie axiale verticale puis rotation de 90 degres *)
  (* BLUE - rotation de -90degres et symetrie axiale verticale *)
  (* GREEN - rotation de 90 degres *)

  (* les combinaisons représentes les faces ; si une coordonnée est défini alors on prend tous les cubies comprenant cette coordonnée en fesant varier les 2 autres coos de 0 à sides-1 *)
  let combs = [|(Some 0 , None , None) ; (Some 2 , None , None ) ; (None , Some 0 , None) ; (None , Some 2 , None)  ; (None , None, Some 0) ; (None , None , Some 2)|] in
  for i = 0 to 5 do
    for co_variable1 = 0 to 3-1 do
      for co_variable2 = 0 to 3-1 do
        match combs.(i) with
        | Some 0, None, None -> print_char (char_color_of_cubie c.(0).(co_variable1).(2-co_variable2) 0) (* RED - "2-" pour symetrie axiale verticale *)
        | Some 2, None, None -> print_char (char_color_of_cubie c.(2).(co_variable1).(co_variable2) 0) (* ORANGE *)

        | None, Some 0, None -> let x1, x2 = rotation90 co_variable1 co_variable2 in Printf.printf "avant rotation : x : %i y : %i c : %c\n" co_variable1 co_variable2 (char_color_of_cubie c.(co_variable1).(0).(co_variable2) 1);
            Printf.printf "après rotation : x : %i y : %i c : %c\n\n" x1 x2 (char_color_of_cubie c.(x1).(0).(x2) 1) ; print_char (char_color_of_cubie c.(x1).(0).(x2) 1) (* WHITE *)
        | None, Some 2, None -> print_char (char_color_of_cubie c.(co_variable1).(2).(co_variable2) 1) (* YELLOW *)

        | None, None, Some 0 -> print_char (char_color_of_cubie c.(co_variable1).(co_variable2).(0) 2) (* BLUE *)
        | None, None, Some 2 -> print_char (char_color_of_cubie c.(co_variable1).(co_variable2).(2) 2) (* GREEN *)

        | _ -> ();
        print_string " "
      done;
      print_newline()
    done;
    print_newline()
  done;
  print_endline "===================================================================================="

let print_option_array (a:'a option array) : unit =
  for i = 0 to Array.length a -1 do
    if Option.is_some a.(i) then
      print_string (get_color (Option.get a.(i)))
    else
      print_string "None";
    print_string " "
  done

let copy_cube (c:cube) : cube =
  (* renvoie une copie de c *)
  let moves = Array.length c.(0) in
  let copy = create_cube moves in
  for x = 0 to moves -1 do
    for y = 0 to moves -1 do
      for z = 0 to moves -1 do
        copy.(x).(y).(z) <- c.(x).(y).(z)
      done;
    done;
  done;
  copy

let exchange_edge_colors (a:color option array) (dim:char): color option array =
  (* une couleur est fixe, les deux autres s'échangent lors de la rotation d'une face
   * dim est la dimension fixe *)
  let c1, c2, c3 = Option.get a.(0), Option.get a.(1), Option.get a.(2) in (* aucun élement ne vaut none car un edge a 3 couleurs *)
  match dim with
  | 'x' -> [|Some c1 ; Some c3 ; Some c2|]
  | 'y' -> [|Some c3 ; Some c2 ; Some c1|]
  | 'z' -> [|Some c2 ; Some c1 ; Some c3|] | _ -> failwith "mauvaise dim renseignée dans exchange_edge_color"

let exchange_vertices_colors (a:color option array) (dim:char): color option array =
  match dim with
  | 'x' -> [|a.(0) ; a.(2) ; a.(1)|]
  | 'y' -> [|a.(2) ; a.(1) ; a.(0)|]
  | 'z' -> [|a.(1) ; a.(0) ; a.(2)|] | _ -> failwith "mauvaise dim renseignée dans exchange_vertices_colors"

let move (c:cube) (value_co_fixe:int) (rank_co_fixe:int): unit =
  (* dim : la dimension qui reste stable ('x', 'y' ou 'z')
   * min indique si on est sur la face d'abscisse 0 (min) ou d'abscisse sides (max) *)

  (* on effectue le mouvement gardant la face de dimension dim qui est min sur c *)

  (* les dimensions qui varient  *)
  let pos_succ_edges = [|(0, 0) ; (0, 2) ; (2, 2) ; (2, 0) ; (0, 0)|] (* on remet la 1ere comb à la fin pour ne pas avoir à gerer de cas particuliers dans notre boucle *)
  and pos_succ_verti = [|(0, 1) ; (1, 2) ; (2, 1) ; (1, 0) ; (0, 1)|] in
  let copy_c = copy_cube c in (* on copie le cube afin de ne plus avoir à mémoriser ce qu'il y avait avant. On pourrait optimiser et ne garder qu'un cubie en mémoire.*)

  if rank_co_fixe = 0 then
    begin
  (* edges  *)
  for i = 0 to 3 do
    let y0, z0 = pos_succ_edges.(i) (* coordonnées actuelles du cubie *)
    and y1, z1 = pos_succ_edges.(i+1) in (* futures coordonnées du cobie *)
    c.(value_co_fixe).(y0).(z0) <- copy_c.(value_co_fixe).(y1).(z1) ;
    c.(value_co_fixe).(y0).(z0) <- exchange_edge_colors c.(value_co_fixe).(y0).(z0) 'x'
  done;
  (* vertices *)
  for i = 0 to 3 do
    let y0, z0 = pos_succ_verti.(i) (* coordonnées actuelles du cubie *)
    and y1, z1 = pos_succ_verti.(i+1) in (* futures coordonnées du cobie *)
    c.(value_co_fixe).(y0).(z0) <- copy_c.(value_co_fixe).(y1).(z1) ;
    c.(value_co_fixe).(y0).(z0) <- exchange_vertices_colors c.(value_co_fixe).(y0).(z0) 'x'
  done
    end
  else if rank_co_fixe = 1 then
    begin
      (* edges  *)
  for i = 0 to 3 do
    let x0, z0 = pos_succ_edges.(i) (* coordonnées actuelles du cubie *)
    and x1, z1 = pos_succ_edges.(i+1) in (* futures coordonnées du cobie *)
    c.(x0).(value_co_fixe).(z0) <- copy_c.(x1).(value_co_fixe).(z1) ;
    c.(x0).(value_co_fixe).(z0) <- exchange_edge_colors c.(x0).(value_co_fixe).(z0) 'y'
  done;
  (* vertices *)
  for i = 0 to 3 do
    let x0, z0 = pos_succ_verti.(i) (* coordonnées actuelles du cubie *)
    and x1, z1 = pos_succ_verti.(i+1) in (* futures coordonnées du cobie *)
    c.(x0).(value_co_fixe).(z0) <- copy_c.(x1).(value_co_fixe).(z1) ;
    c.(x0).(value_co_fixe).(z0) <- exchange_vertices_colors c.(x0).(value_co_fixe).(z0) 'y'
  done
    end
  else
    begin
  (* edges  *)
  for i = 0 to 3 do
    let x0, y0 = pos_succ_edges.(i) (* coordonnées actuelles du cubie *)
    and x1, y1 = pos_succ_edges.(i+1) in (* futures coordonnées du cobie *)
    c.(x0).(y0).(value_co_fixe) <- copy_c.(x1).(y1).(value_co_fixe) ;
    c.(x0).(y0).(value_co_fixe) <- exchange_edge_colors c.(x0).(y1).(value_co_fixe) 'z'
  done;
  (* vertices *)
  for i = 0 to 3 do
    let x0, y0 = pos_succ_verti.(i) (* coordonnées actuelles du cubie *)
    and x1, y1 = pos_succ_verti.(i+1) in (* futures coordonnées du cobie *)
    c.(x0).(y0).(value_co_fixe) <- copy_c.(x1).(y1).(value_co_fixe) ;
    c.(x0).(y0).(value_co_fixe) <- exchange_vertices_colors c.(x0).(y0).(value_co_fixe) 'z'
  done
    end

let move_R' (c:cube) : unit = move c 2 0
let move_R (c:cube) : unit = move_R' c ; move_R' c ; move_R' c

let move_L (c:cube) : unit = move c 0 0
let move_L' (c:cube) : unit = move_L c ; move_L c ; move_L c

let move_U' (c:cube) : unit = move c 0 1
let move_U (c:cube) : unit = move_U' c ; move_U' c ; move_U' c

let move_D (c:cube) : unit = move c 2 1
let move_D' (c:cube) : unit = move_D c ; move_D c ; move_D c

let move_F (c:cube) : unit = move c 2 2
let move_F' (c:cube) : unit = move_F c ; move_F c ; move_F c

let move_B' (c:cube) : unit = move c 0 2
let move_B (c:cube) : unit = move_B' c ; move_B' c ; move_B' c

let rec exec_sequence (l:string list) (c:cube) : unit =
  (* exécute une série de mouvements sur le cube c *)
  match l with
  | [] -> ()
  | t::q ->
    match t with
    | "R" -> move_R c | "R'" -> move_R' c
    | "L" -> move_L c | "L'" -> move_L' c
    | "U" -> move_U c | "U'" -> move_U' c
    | "D" -> move_D c | "D'" -> move_D' c
    | "F" -> move_F c | "F'" -> move_F' c
    | "B" -> move_B c | "B'" -> move_B' c | _ -> ()
let _ =
  let mon_cube = create_cube 3 in
  (*exec_sequence ["U" ; "R" ; "D'" ; "R'" ; "U'" ; "F'" ; "B" ; "L'" ; "D'" ; "F" ; "L'" ; "B'"] mon_cube;*)
  (*  *exec_sequence ["U" ; "R" ; "D'"] mon_cube ;*)
  print_cube_pattern_x3 mon_cube;
  move_U mon_cube;
  move_R mon_cube;
  move_D' mon_cube;
  print_cube_pattern_x3 mon_cube
