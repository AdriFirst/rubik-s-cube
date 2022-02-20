
type color = White | Red | Blue | Orange | Green | Yellow
type colors = Some of color | Some of (color,color) | color, color, color
type cubies =
    Faces of colors
  | Vertices of colors
  | Edges of colors
type cube = cubies array

let create_cube (sides : int) : cubies array =
  let cube = Array.make 27 (Faces (Some White)) in
  for x = 0 to sides-1 do
    for y = 0 to sides-1 do
      for z = 0 to sides-1 do
        if (x = 0 || x = sides-1) && (y = 0 || y = sides-1) && (z = 0 || z = sides-1) then
          (* coin *)
          cube.(x*sides*sides + y*sides + z) <- Edges ( White White White)
