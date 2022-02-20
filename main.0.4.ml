
type color = White | Red | Blue | Orange | Green | Yellow
type colors = color option array
    (*représentes les couleurs visibles du cubie (min 0 (invisible for outside), max 3 (Vertices))*)
type cubie =
    Faces of colors | Vertices of colors | Center of colors (* invisible from outside *) | Edges of colors

type cube = cubie array array array

let create_cube (sides : int) : cube =
  let cube = Array.make sides [||] in for i = 0 to sides-1 do cube.(i) <- Array.make_matrix sides sides (Center [|None;None;None|]) done;
  for x = 0 to sides-1 do
    for y = 0 to sides-1 do
      for z = 0 to sides-1 do
        if (x = 0 || x = sides-1) && (y = 0 || y = sides-1) && (z = 0 || z = sides-1) then
          (* edge *)
          cube.(x).(y).(z) <- Edges ([|Some White;Some Blue;Some Red|])
        else if ((x = 0 || x = sides-1) && (y = 0 || y = sides-1)) || ((y = 0 || y = sides-1) && (z = 0 || z = sides-1)) || ((x = 0 || x = sides-1) && (z = 0 || z = sides-1)) then
          (* vertices *)
          cube.(x).(y).(z) <- Vertices [|Some Red;Some Blue;None|]
        else if (x = 0 || x = sides-1) || (y = 0 || y = sides-1) || (z = 0 || z = sides-1) then
          (* faces *)
          cube.(x).(y).(z) <- Faces [|Some Green;None;None|]
        else
          (* center(s) *)
          cube.(x).(y).(z) <- Center [|None;None;None|]
      done;
    done;
  done;
  cube

let index_of_some (a : 'a option array) : int =
  (* renvoie l'indice du 1er élement valant "some" d'un 'a option array*)
  let res = ref (-1) in
  for i = 0 to Array.length a -1 do
    if Option.is_some a.(i) && !res = -1 then
      res := i
  done;
  if !res = -1 then failwith "pas de variable some dans ce tableau" else !res

let index_of_nones (a : 'a option array) : int*int =
  (* renvoie l'indice des 2ers élement valant "none" d'un 'a option array*)
  let res = ref (-1,-1) in
  for i = 0 to Array.length a -1 do
    if Option.is_none a.(i) then
      if fst !res <> -1 then
        res := i,-1
      else
        let x,y = !res in res := x,i
  done;
  if snd !res = -1 then failwith "moins de deux variables None dans ce tableau" else !res

let get_index_of_values (tab : 'a option array) : int*int*int=
  (* permet de former le couple de possibilités pour l'affichage d'une face du cube *)
  let a,b = index_of_nones tab
  and c = index_of_some tab in
  c,a,b

let string_color_of_cubie (cubie:cubie) (coordonate:int): string =
  (* coordonate est l'axe sur lequel est étendu la case couleur  *)
  match cubie with
  | Faces tab when (Option.get tab.(coordonate)) = White -> "W" | Faces tab when (Option.get tab.(coordonate)) = Yellow -> "Y"
  | Faces tab when (Option.get tab.(coordonate)) = Orange -> "O"| Faces tab when (Option.get tab.(coordonate)) = Red -> "R"
  | Faces tab when (Option.get tab.(coordonate)) = Green -> "G" | Faces tab when (Option.get tab.(coordonate)) = Blue -> "B"

  | Vertices tab when (Option.get tab.(coordonate)) = White -> "W" | Vertices tab when (Option.get tab.(coordonate)) = Yellow -> "Y"
  | Vertices tab when (Option.get tab.(coordonate)) = Orange -> "O"| Vertices tab when (Option.get tab.(coordonate)) = Red -> "R"
  | Vertices tab when (Option.get tab.(coordonate)) = Green -> "G" | Vertices tab when (Option.get tab.(coordonate)) = Blue -> "B"

  | Edges tab when (Option.get tab.(coordonate)) = White -> "W" | Edges tab when (Option.get tab.(coordonate)) = Yellow -> "Y"
  | Edges tab when (Option.get tab.(coordonate)) = Orange -> "O"| Edges tab when (Option.get tab.(coordonate)) = Red -> "R"
  | Edges tab when (Option.get tab.(coordonate)) = Green -> "G" | Edges tab when (Option.get tab.(coordonate)) = Blue -> "B"

  | Center tab -> failwith "ce cubie est invisible, il n'a donc pas de couleur visible" | _ -> failwith "unmatched in string_color_of_cubie"

let print_cube_pattern_x3 (c:cube) : unit =
  let comb = [|[|Some 0 ; None ; None|]; [|Some 2 ; None ; None |] ;  [|None ; None; Some 0|] ; [|None ; None ; Some 2|] ; [|None ; Some 0 ; None|] ; [|None ; Some 2 ; None|] |] in
  for i = 0 to 5 do
    (* On choisit une face cad deux coordonnées maximales et minimales et affichons les 9 cubies correpondant *)
    let i_co1, i_co2, i_co3 = get_index_of_values comb.(i) in (* i_co1 : valeur fixee, i_co2 et i_co3 : valeurs variables *)
    if  i_co1 = 0 then
      (* x est fixé  *)
      for y = 0 to 3-1 do
        for z = 0 to 3-1 do
          let cubie = c.(Option.get comb.(i)..(y).(z) in
          print_string (string_color_of_cubie cubie i_co1) ; print_string " "

    else if Option.is_some comb.(i).(1) then
      (* y est fixé  *)
    else if Option.is_some comb.(i).(2) then
      (* z est fixé  *)
    else failwith "error while printing cube patterns"
