#!/usr/bin/env python3.8
from math import pi, cos, sin

def next_pos (x0,y0, a):
    return x0 * cos (a) - y0 * sin (a) , x0 * sin (a) + y0 * cos (a)
    # return (1 + i * cos(angle) - j * sin(angle) , 1 + i * sin(angle) + j * cos(angle))

for i in range(3):
    for j in range(3):
        next = next_pos (i, j, pi/2)
        print(f"{i} {j} | {next[0]} {next[1]}")
#let pos_after_rot (x:int) (y:int) (angle:float) (sides:int) : int*int =
  # (* Returns the position of a cubies with the coordonates (x, y) after a rotation of angle radian.
  #  * sides : cote of our rubik's cube
  #  * real center : (-sides/2, -sides/2)
  #  * We wants to realise a rotation of a face, mean in function of the center of this face, but since in our notation system the origine is a corner, we consider the decalage by renseigning the coordonnee of the real origin*)
  # (* x' = x0 + (x-x0) cos(theta), y' = y0 + (y-y0) sin(theta) where  (x0,y0) is the real center, and theta the angle of rotation. Credit to M. de Falco : http://marc.de-falco.fr/ *)
  # print_endline "begin rotation pos" ;
  # 1 + (x*1) * int_of_float (cos angle), 1 + (y*1) * int_of_float (sin angle)
