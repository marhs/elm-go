-- Go board in Elm
-- Marco Herrero <me@marhs.de>

import Signal (Signal, sampleOn, foldp, (<~))
import Signal
import Mouse
import Graphics.Collage (..)
import Graphics.Element (..)
import List (map, filter)
import Color
import Array (Array)
import Array
import Maybe (withDefault, Maybe (Just, Nothing))

-----------
-- Model --
-----------

type alias State = { stones : Array Int
                   , points : (Int,Int) 
                   , nextTurn : Int 
                   }

-- The initial states contains an array filled with "-1"
-- If a stone is placed, it's changed to 0 (black) or 1 (white)
initialState : State
initialState = { stones = Array.repeat (19 * 19) -1
               , points = (0,0) 
               , nextTurn = 0 
               }

------------
-- Update --
------------

click : (Int, Int) -> State -> State
click (x,y) state = 
    if | (x > 18) || (y > 18) -> state 
       | (withDefault -1 (Array.get (19 * y + x) state.stones)) /= -1 -> changeColor x y state
       | otherwise ->
            let ix = (19 * y + x) 
                stones' = Array.set ix state.nextTurn state.stones 
                nextTurn' = (state.nextTurn + 1) % 2
                points' = state.points
            in { stones = stones', points = points', nextTurn = nextTurn' }

changeColor : Int -> Int -> State -> State
changeColor a b state =
  let stone x y = (withDefault -1 (Array.get (19 * y + x) state.stones))
      stone' = ((stone a b) % 3) - 1
      ix = (19 * b + a)
      stones' = Array.set ix stone' state.stones 
  in { state | stones <- stones' }

----------
-- View --
----------

display : State -> Element
display s = collage 600 600 <| grid ++ drawStones s.stones 
 
drawStones : Array Int -> List Form
drawStones stones = 
    let s x = withDefault 0 (Array.get x stones)
        drawStone = \a -> ( stone (a % 19) (a // 19) (s a) )
    in map drawStone (filter (\x -> (s x)>=0) [0..(19 * 19 - 1)])

-- Draws a stone at x y (go coordinates, not pixels) with color 0 or 1
--      draw 0 0 1 = draws a white stone at the bottom left corner
stone : Int -> Int -> Int -> Form
stone x y c =
    let p a = toFloat (-270 + 30*a)
        co =
          if | c == 0 -> Color.black
             | otherwise -> Color.white
        fill = move (p x, p y) <| filled co <| circle 15
        border = move (p x, p y) <| outlined (solid Color.black) <| circle 15
    in group [fill, border]


-- Draw the board lines
-- The distances are hardcoded: 30px between lines and 540px the full board
grid : List Form
grid =
    let segV s i = traced (solid Color.black) <| segment (i,-s/2) (i,s/2)
        segH s i = traced (solid Color.black) <| segment (-s/2,i) (s/2,i)
        coords = map (\x -> x*30) [-9..9]
        cPoints = [(0,0)
                  ,(-180,0),(180,0),(0,-180),(0,180)
                  ,(-180,-180),(-180,180),(180,-180),(180,180)
                  ]
        fill (x,y) = move (x, y) <| filled Color.black <| circle 4
    in (map (\x -> (segV 540 x)) coords) ++
       (map (\x -> (segH 540 x)) coords) ++
       (map fill cPoints)
main : Signal Element
main = display <~ (foldp click initialState input)

-------------
-- Signals --
-------------

-- Transform from pixels to coordinates
toCoords : (Int,Int) -> (Int,Int)
toCoords (x,y) =
    let x' = (x - 15) // 30
        y' = (600 - y - 15) // 30
    in (x', y')

-- Update with the coordinates when clicked
input : Signal (Int, Int)
input = sampleOn Mouse.clicks (Signal.map toCoords Mouse.position)
