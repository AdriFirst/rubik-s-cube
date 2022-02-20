
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

let pos_after_rot (x:int) (y:int) (angle:float) (sides:int) : int*int =
  (* Returns the position of a cubies with the coordonates (x, y) after a rotation of angle radian.
   * sides : cote of our rubik's cube
   * real center : (-sides/2, -sides/2)
   * We wants to realise a rotation of a face, mean in function of the center of this face, but since in our notation system the origine is a corner, we consider the decalage by renseigning the coordonnee of the real origin*)
  (* x' = x0 + (x-x0) cos(theta), y' = y0 + (y-y0) sin(theta) where  (x0,y0) is the real center, and theta the angle of rotation. Credit to M. de Falco : http://marc.de-falco.fr/ *)
  print_endline "begin rotation pos" ;
  (-1) + (x*(-1)) * int_of_float (cos angle), (-1) + (y*(-1)) * int_of_float (sin angle)


let move (c:cube) (angle:float) (dimension : int) (face : int) : unit =
  (* We realise the rotation `angle` on the face caracterised by a dimension (0/1/2 for x/y/z) and the length of this dimension : face (the most / least near from the real center) *)

  let sides = Array.length c.(0)
  and copy_c = copy_cube c in (* We make a copy of the cube to easily take new values without taking the one just before *)

  for co_variable1 = 0 to sides-1 do
    for co_variable2 = 0 to sides-1 do
      (* les coordonées obtenues après rotation :*)
      let cov1next, cov2next = pos_after_rot co_variable1 co_variable2 angle sides in
      print_endline "end rotation pos";
      Printf.printf "cov1 %i | cov2 %i | cov1n %i | cov2n %i \n" co_variable1 co_variable2 cov1next cov2next;
      (* on s'en fiche de l'ordre co1 / co2 ; on veut juste couvrir toutes les possibilités *)
      match dimension with
      | 0 -> c.(face).(co_variable1).(co_variable2) <- copy_c.(face).(cov1next).(cov2next) (* X est fixe *)
      | 1 -> c.(co_variable1).(face).(co_variable2) <- copy_c.(cov1next).(face).(cov2next) (* Y est fixe *)
      | 2 -> c.(co_variable1).(co_variable2).(face) <- copy_c.(cov1next).(cov2next).(face) (* Z est fixe *)
      | _ -> failwith "OCaml have limits"
    done
  done
let _ =
  let mon_cube = create_cube 3 in
  print_cube_pattern_x3 mon_cube;
  move mon_cube (Float.pi /.2.) 0 2;
  print_cube_pattern_x3

  (* CACA CAR ON PEUT EFFECTUER DES ROTATIONS DE 180 DEGRES DU COUP PLUS QUE 2 ELTS PAR COUPLE
   *  *for rings = 0 to sides/2 -1 do (* we decompose a face with bordures like squares in squares. Usefull here because we mind couples *)
    for couples_number = rings to sides-2 do (* there is sides-1 * 4 cubies in bordure of each squares *)
      for couple = 0 to 3 do (* apply this for each elt of couple *)
        let x, y = couples_number, rings in (* please have some imagination *)
        let x2, y2 = pos_after_rot x y
        match dimension with
        | 0 -> c.(face).() (* X est fixe *)
        | 1 -> (* Y est fixe *)
        | 2 -> (* Z est fixe *)*)
(*let move_r (c:cube) : unit =
  (* on effectue le mouvement R sur c *)

  let sides = 3
  and copy_c = copy_cube c in (* on copie le cube afin de ne plus avoir à mémoriser ce qu'il y avait avant *)

  (* par rapport au centre, les rotations seraient : M(x,y) -> M' (y, -x) *)
  (* par rapport au coin supérieur gauche cela donne : M(x, y) -> M' (y-sides/2, -x-sides/2) *)

  for couple_nb = 0 to sides-2 do (* il y a cote-1 * 4 cubies de en bordure -- attention gerer les cubes faces si sides > 3 -- *)
    for i_elt_of_couple = 0 to 3 do (* on bouge un à un les élements du couple *)

      print_newline();
      (* dans le cas du mouvement R, x0 est stable, seuls y0 et z0 varient *)
      let x0, y0, z0 = sides-1, i_elt_of_couple, couple_nb in
      let y1,z1 = new_cos_rot90 y0 z0 in
      c.(x0).(y0).(z0) <- copy_c.(x0).()

    done;
  done

let _ =
  let mon_cube = create_cube 3 in
  move_r mon_cube;
  print_cube_pattern_x3
*)
