
type color = White | Red | Blue | Orange | Green | Yellow
type colors = color list
type cubies =
    Faces of colors | Vertices of colors | Center (* invisible from outside *) | Edges of colors

type cube = cubies array array array

let create_cube (sides : int) : cube =
  let cube = Array.make sides [||] in for i = 0 to sides-1 do cube.(i) <- Array.make_matrix sides sides (Center true) done;
  for x = 0 to sides-1 do
    for y = 0 to sides-1 do
      for z = 0 to sides-1 do
        if (x = 0 || x = sides-1) && (y = 0 || y = sides-1) && (z = 0 || z = sides-1) then
          (* edge *)
          cube.(x).(y).(z) <- Edges ([White;Blue;Red])
        else if ((x = 0 || x = sides-1) && (y = 0 || y = sides-1)) || ((y = 0 || y = sides-1) && (z = 0 || z = sides-1)) || ((x = 0 || x = sides-1) && (z = 0 || z = sides-1)) then
          (* vertices *)
          cube.(x).(y).(z) <- Vertices [Red;Blue]
        else if (x = 0 || x = sides-1) || (y = 0 || y = sides-1) || (z = 0 || z = sides-1) then
          (* faces *)
          cube.(x).(y).(z) <- Faces [Green]
        else
          (* center(s) *)
          cube.(x).(y).(z) <- Center
      done;
    done;
  done;
  cube

let index_of_none (a : 'a option array) : int =
  (* renvoie l'indice du 1er élement valant "none" d'un 'a option array*)
  let res = ref (-1) in
  for i = 0 to Array.length a -1 do
    if Option.is_none a.(i) then
      res := i
  done;
  if !res = -1 then failwith "pas de variable none dans ce tableau" else !res

let index_of_Somes (a : 'a option array) : int*int =
  (* renvoie l'indice du 1er élement valant "none" d'un 'a option array*)
  let res = ref (-1,-1) in
  for i = 0 to Array.length a -1 do
    if Option.is_some a.(i) then
      if fst !res <> -1 then
        res := i,-1
      else
        let x,y = !res in res := x,i
  done;
  if snd !res = -1 then failwith "moins de deux variables Some dans ce tableau" else !res

let print_cube_pattern_x3 (c:cube) : unit =
  let comb = [|[|Some 0 ; Some 0 ; None|]; [|Some 0 ; Some 2 ; None |] ; [|Some 0 ; None ; Some 0|] ; [|Some 0 ; None ; Some 2|] ; [|None ; Some 0 ; Some 0|] ; [|None ; Some 0 ; Some 2|]|] in
  for i = 0 to 5 do
    (* On choisit une face cad deux coordonnées maximales et minimales et affichons les 9 cubies correpondant *)
    let co1,co2,
