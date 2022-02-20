(* Nous avons un cube 3x3 composé de 27 cubies
 * Ces cubies ont un certain nombre de certaines couleurs, représentés par le type colors.
 * colors = [|None ; None ; None|] -> le cubie est LE centre (invisible de l'exterieur, de coordonnées (0, 0, 0))
 * colors = [|Red ; None ; None |] -> faces (sur une face mais non en bordure)
 * colors = [|Red ; Blue ; None |] -> Vertices (arete)
 * colors = [|Red ; Blue ; White ] -> Edge (coin) *)

(* À noter que color Option représente la dimension (x, y ou z) sur laquelle le cubie est visible. *)
(* Par conséquent, lors d'une rotation du cube, la répartition de ces color Option changent. *)

(* Nous considérons aussi le repère (O, x, y, z). le centre de coordonnée est donc le seul cubie invisible et qui ne change pas de position *)

type color = White | Red | Blue | Orange | Green | Yellow
type colors = color option array
type cube = colors array array array

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

let create_cube (sides : int) : cube =
  (*  *)
