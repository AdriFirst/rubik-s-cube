
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

(*let applique_rotation ()*)
let move_r (c:cube) : unit =
  (* on effectue le mouvement R sur c *)

  let sides = 3
  and copy_c = copy_cube c in (* on copie le cube afin de ne plus avoir à mémoriser ce qu'il y avait avant *)

  (* par rapport au centre, les rotations seraient : M(x,y) -> M' (y, -x) *)
  (* par rapport au coin supérieur gauche cela donne : M(x, y) -> M' (y-sides/2, -x-sides/2) *)

  for couple_nb = 0 to sides-1 do
    for i_elt_of_couple = 0 to 3 do
      (* faute de formule, on effectue une disjonction des cas *)
      print_newline();
      let x0, y0, z0 = sides-1, i_elt_of_couple, couple_nb in
      match i_elt_of_couple with
      | 0 ->  print_int (z0-((sides-1)/2)) ; print_int (-y0-((sides-1)/2)) ;
        c.(x0).(y0).(z0) <- copy_c.(x0).(z0-((sides-1)/2)).(-y0-((sides-1)/2))

      | 1 -> print_int (-y0-((sides-1)/2)-((sides-1)/2)) ; print_int (-z0-((sides-1)/2)+((sides-1)/2)) ;
        c.(x0).(y0).(z0) <- copy_c.(x0).(-y0-((sides-1)/2)-((sides-1)/2)).(-z0-((sides-1)/2)+((sides-1)/2))

      | 2 -> print_int (-z0-((sides-1)/2)) ; print_int (- (-y0-((sides-1)/2)-((sides-1)/2)) - ((sides-1)/2)) ;
        c.(x0).(y0).(z0) <- copy_c.(x0).(-z0-((sides-1)/2)).(- (-y0-((sides-1)/2)-((sides-1)/2)) - ((sides-1)/2))

      | 3 -> print_int y0 ; print_int z0 ;
        c.(x0).(y0).(z0) <- copy_c.(x0).(y0).(z0)

      | _ -> print_string "||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||"

    done;
  done

let _ =
  let mon_cube = create_cube 3 in
  move_r mon_cube;
  print_cube_pattern_x3
