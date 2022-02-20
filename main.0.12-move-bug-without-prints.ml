
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
  for x = 0 to sides-1 do
    for y = 0 to sides-1 do
      for z = 0 to sides-1 do
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

let move_r (c:cube) : unit =
  (* on effectue le mouvement R sur c *)
  (* n.x = (n-1).y(-1)**n
   * n.y = (n-1).x *)
  (* /!\ les valeurs seront temporairement incorrectes  *)
  let sides = 3 in
  (* on va effectuer les permutations 4 à 4, les arrêtes composant un cycle par le mouvement R*)
  (* x = 3-1 car c'est la tranche droite  *)

  (* DEPLACEMENT DES CUBIES ET ROTATION DES COULEURS *)
  for cycles = 0 to sides-1 do

    (* les quatres cubies du cycles sont ceux de coordonnées :  *)
    let cubies = [|(sides-1, 0, cycles) ; (sides-1, cycles, sides-1) ; (sides-1, sides-1, sides-1 -cycles) ; (sides-1, sides-1-cycles, 0)|] in
    let temp_cubies  = let x, y, z = cubies.(0) in c.(x).(y).(z) in (* on garde les couleurs d'un cubies en mémoire lors des permutation (ils vont se remplacer un à un)*)
    for i = 4-1 downto 1 do
      (* cas où on utilise la variable temporaire *)
      if i-1 = 0 then
          let x, y, z = cubies.(i) in c.(x).(y).(z) <- temp_cubies
      else
          let x, y, z = cubies.(i) in let x1, y1, z1 = cubies.(i-1) in c.(x).(y).(z) <- c.(x1).(y1).(z1);

        let c_temp = c.(x).(y).(z).(1) in
        c.(x).(y).(z).(1) <- c.(x).(y).(z).(2);
        c.(x).(y).(z).(2) <- c_temp

    done;
  done


    (*for i = Array.length cubies -1 downto 0 do
       let x1, y1, z1  = cubies.(i) in c.(x1).(y1).(z1) <- let x0, y0, z0 = cubies.(i-1)*)
    (*
    let rec un (n:int) : int*int =
      match n with
      | 0 -> cycles, 0
      | _ -> let x0, y0 = un (n-1) in if n mod 2 = 0 then y0, x0 else -y0, x0
    in




    let temp = c.(sides-1).(cycles).(0) in
    c.(sides-1).(cycles).(0) <- let y,z = un 3 in c.(3-1).(y).(z);
    c.(sides-1).(sides-1).(cycles) <-
    *)


    (* let temp = c.(3-1).(couples).(0) in
       c.(3-1).(couples).(0) <- c.(3-1).(0).()*)
