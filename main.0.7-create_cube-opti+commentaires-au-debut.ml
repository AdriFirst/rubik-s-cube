
(* Nous avons un cube composé de sides**3 cubies, sides étant le nombre de cubies composant le côté du cube.
 * Ces cubies ont un certain nombre de certaines couleurs, représentés par le type colors.
 * colors = [|None ; None ; None|] -> le cubie est un centre (invisible de l'exterieur)
 * colors = [|Red ; None ; None |] -> faces (sur une face mais non en bordure)
 * colors = [|Red ; Blue ; None |] -> Vertices (arete)
 * colors = [|Red ; Blue ; White ] -> Edge (coin) *)

(* À noter que color Option représente la dimension (x, y ou z) sur laquelle le cubie est visible. *)
(* Par conséquent, lors d'une rotation du cube, la répartition de ces color Option changent. *)

type color = White | Red | Blue | Orange | Green | Yellow
type colors = color option array
type cube = colors array array array

let create_cube (sides : int) : cube =
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
    if Option.is_some comb.(i).(0) then
      for y = 0 to 3-1 do
        for z = 0 to 3-1 do
          let cubie = c.(Option.get comb.(i).(0)).(y).(z) in
          print_string (string_color_of_cubie cubie (Option.get comb.(i).(0))) ; print_string " "
        done;
        print_newline()
      done
    else if Option.is_some comb.(i).(1) then
      for x = 0 to 3-1 do
        for z = 0 to 3-1 do
          let cubie = c.(x).(Option.get comb.(i).(1)).(z) in
          print_string (string_color_of_cubie cubie (Option.get comb.(i).(1))) ; print_string " "
        done;
        print_newline()
      done
    else if Option.is_some comb.(i).(2) then
      for x = 0 to 3-1 do
        for y = 0 to 3-1 do
          let cubie = c.(x).(y).(Option.get comb.(i).(2)) in
          print_string (string_color_of_cubie cubie (Option.get comb.(i).(2))) ; print_string " "
        done;
        print_newline()
      done
    else failwith "can't print cube pattern";
    print_newline()
  done
