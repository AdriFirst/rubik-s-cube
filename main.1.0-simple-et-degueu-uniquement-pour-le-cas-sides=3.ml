
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

  (* les combinaisons représentes les faces ; si une coordonnée est défini alors on prend tous les cubies comprenant cette coordonnée en fesant varier les 2 autres coos de 0 à sides-1 *)
  let combs = [|(Some 0 , None , None) ; (Some 2 , None , None ) ; (None , Some 0 , None) ; (None , Some 2 , None)  ; (None , None, Some 0) ; (None , None , Some 2)|] in
  for i = 0 to 5 do
    for co_variable1 = 0 to 3-1 do
      for co_variable2 = 0 to 3-1 do
        match combs.(i) with
        | Some v, None, None -> print_char (char_color_of_cubie c.(v).(co_variable1).(co_variable2) 0) (* 0 pour x *)
        | None, Some v, None -> print_char (char_color_of_cubie c.(co_variable1).(v).(co_variable2) 1) (* 1 pour y *)
        | None, None, Some v -> print_char (char_color_of_cubie c.(co_variable1).(co_variable2).(v) 2) (* 2 pour z *)
        | _ -> ();
        print_string " "
      done;
      print_newline()
    done;
    print_newline()
  done

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

let exchange_coin_colors (a:color option array) (dim:char): color option array =
  (* une couleur est fixe, les deux autres s'échangent lors de la rotation d'une face
   * dim est la dimension fixe *)
  let c1, c2, c3 = Option.get a.(0), Option.get a.(1), Option.get a.(2) in (* aucun élement ne vaut none car un coin a 3 couleurs *)
  match dim with
  | 'x' -> [|Some c1 ; Some c3 ; Some c2|]
  | 'y' -> [|Some c3 ; Some c2 ; Some c1|]
  | 'z' -> [|Some c2 ; Some c1 ; Some c3|] | _ -> failwith "mauvaise dim renseignée dans exchange_coin_color"

let exchange_vertices_colors (a:color option array) (dim:char): color option array =
  match dim with
  | 'x' -> [|a.(0) ; a.(2) ; a.(1)|]
  | 'y' -> [|a.(2) ; a.(1) ; a.(0)|]
  | 'z' -> [|a.(1) ; a.(0) ; a.(2)|] | _ -> failwith "mauvaise dim renseignée dans exchange_vertices_colors"

let move_r' (c:cube) : unit =
  (* on effectue le mouvement R sur c *)

  let copy_c = copy_cube c in (* on copie le cube afin de ne plus avoir à mémoriser ce qu'il y avait avant *)
  (* coins  *)
  c.(2).(0).(0) <- copy_c.(2).(0).(2); c.(2).(0).(0) <- exchange_coin_colors c.(2).(0).(0) 'x' ;
  c.(2).(0).(2) <- copy_c.(2).(2).(2); c.(2).(0).(2) <- exchange_coin_colors c.(2).(0).(2) 'x' ;
  c.(2).(2).(2) <- copy_c.(2).(2).(0); c.(2).(2).(2) <- exchange_coin_colors c.(2).(2).(2) 'x' ;
  c.(2).(2).(0) <- copy_c.(2).(0).(0); c.(2).(2).(0) <- exchange_coin_colors c.(2).(2).(0) 'x' ;

  (* aretes *)
  c.(2).(0).(1) <- copy_c.(2).(1).(2); c.(2).(0).(1) <- exchange_vertices_colors c.(2).(0).(1) 'x' ;
  c.(2).(1).(2) <- copy_c.(2).(2).(1); c.(2).(1).(2) <- exchange_vertices_colors c.(2).(1).(2) 'x' ;
  c.(2).(2).(1) <- copy_c.(2).(1).(0); c.(2).(2).(1) <- exchange_vertices_colors c.(2).(2).(1) 'x' ;
  c.(2).(1).(0) <- copy_c.(2).(0).(1); c.(2).(1).(0) <- exchange_vertices_colors c.(2).(1).(0) 'x'
