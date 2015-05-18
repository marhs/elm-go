-- Go board in Elm
-- Marco Herrero <me@marhs.de>

import Signal exposing (Signal, sampleOn, foldp, (<~))
import Signal
import Mouse
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import List exposing (map, filter)
import Color
import Array exposing (Array)
import Array
import Maybe exposing (withDefault, Maybe (Just, Nothing))

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

-- Modifies a state adding a stone
click : (Int, Int) -> State -> State
click (x,y) state =
    if | (x > 18) || (y > 18) -> state
       | (withDefault -1 (Array.get (19 * y + x) state.stones)) /= -1 -> deleteStone x y state
       | otherwise ->
            let ix = (19 * y + x)
                stones' = Array.set ix state.nextTurn state.stones
                nextTurn' = (state.nextTurn + 1) % 2
                points' = state.points
            in { stones = stones', points = points', nextTurn = nextTurn' }

-- Delete a placed stone
deleteStone : Int -> Int -> State -> State
deleteStone a b state =
  let stone x y = (withDefault -1 (Array.get (19 * y + x) state.stones))
      ix = (19 * b + a)
      stones' = Array.set ix -1 state.stones
  in { state | stones <- stones' }

-- Check if a stone is dead
-- Place a stone (a, b) and check if one of the surroundings stones are dead
-- TODO
checkStone : Int -> Int -> State -> State
checkStone a b state = state

-- Give a position in the board, return a list with all then position of that
-- stone
-- TODO
buildStone : Int -> Int -> Maybe (List (Int, Int))
buildStone a b = Just [(1,2),(2,3)]

-- I have to implement a (functional) recursive BFS
buildStoneR : Int -> Int -> List (Int, Int) -> List (Int, Int)
buildStoneR a b acc = [(1,2),(2,3)]

-- Return the liberties of a stone
-- TODO
liberties : List (Int, Int) -> State -> Int
liberties a s = 0
    --let adj = [(0,1), (1,0), (0, -1), (-1, 0)]

adjacent : Int -> Int -> List (Int, Int)
adjacent x y = 
    let adj = [(0,1), (1,0), (0, -1), (-1, 0)]
        p1 = filter (\(a,b) -> (a >= 0) && (a < 19) && (b >= 0) && (b < 19) ) 
        p2 = map (\(a, b) -> (a+x, b+y)) adj
    in p1 p2

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
