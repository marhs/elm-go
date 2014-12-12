-- Go board in Elm
-- Marco Herrero <me@marhs.de>

import Graphics.Collage (..)
import Graphics.Element (..)

import List (map)
import Color

-- Draw the board lines
-- The distances are hardcoded: 30px between lines and 540px the full board
grid : List Form
grid =
    let segV s i = traced (solid Color.black) <| segment (i,-s/2) (i,s/2)
        segH s i = traced (solid Color.black) <| segment (-s/2,i) (s/2,i)
        coords = map (\x -> x*30) [-9..9]
    in (map (\x -> (segV 540 x)) coords) ++
       (map (\x -> (segH 540 x)) coords)

-- Draws a stone at x y (go coordinates, not pixels) 
--      draw 0 0 = draws a stone at the bottom left corner
stone : Int -> Int -> Form
stone x y =
    let p a = toFloat (-270 + 30*a)
        fill = move (p x, p y) <| filled Color.white <| circle 15
        border = move (p x, p y) <| outlined (solid Color.black) <| circle 15
    in group [fill, border]

-- Append the stones to the list
main : Element
main = collage 600 600 <| grid ++ [stone 0 0] ++ [stone 4 5]
