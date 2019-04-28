module Board exposing
    ( Board
    , beginner
    , normal
    , pos
    , size
    , small
    )

import Array exposing (Array)
import Stone exposing (Stone(..))


type Board
    = Board Int (Array (Maybe Stone))


normal : Board
normal =
    new 19


small : Board
small =
    new 13


beginner : Board
beginner =
    new 9


size : Board -> Int
size board =
    case board of
        Board x _ ->
            x


pos : Int -> Int -> Board -> Maybe Stone
pos x y board =
    let
        i =
            indexOf x y board
    in
    case board of
        Board _ positions ->
            Maybe.withDefault Nothing (Array.get i positions)



-- Internal


new : Int -> Board
new x =
    Board x (Array.repeat (x * x) Nothing)


indexOf : Int -> Int -> Board -> Int
indexOf x y board =
    x + y * size board
