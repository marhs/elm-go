-- Go board in Elm
-- Marco Herrero <me@marhs.de>

import Signal (Signal, sampleOn, foldp, (<~))
import Signal
import Mouse
import Text (asText)

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

-- The initial state contains the zero for each possible roll
-- Starts with a seed of 42 (changing this will change the outcome of the sequence)
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
       | (withDefault -1 (Array.get (19 * y + x) state.stones)) /= -1 -> state
       | otherwise ->
            let ix = (19 * y + x) 
                stones' = Array.set ix state.nextTurn state.stones 
                nextTurn' = (state.nextTurn + 1) % 2
                points' = state.points
            in { stones = stones', points = points', nextTurn = nextTurn' }

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
stone : Int -> Int -> Int -> Form
stone x y c =
    let p a = toFloat (-270 + 30*a)
        co =
          if | c == 0 -> Color.black
             | otherwise -> Color.white
        fill = move (p x, p y) <| filled co <| circle 15
        border = move (p x, p y) <| outlined (solid Color.black) <| circle 15
    in group [fill, border]


-- Append the stones to the list
main : Signal Element
--main = display initialState
main = display <~ (foldp click initialState input)

-------------
-- Signals --
-------------


toCoords : (Int,Int) -> (Int,Int)
toCoords (x,y) =
    let x' = (x - 15) // 30
        y' = (600 - y - 15) // 30
    in (x', y')

input : Signal (Int, Int)
input = sampleOn Mouse.clicks (Signal.map toCoords Mouse.position)



