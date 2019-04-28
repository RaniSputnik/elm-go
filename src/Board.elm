module Board exposing
    ( Board
    , beginner
    , normal
    , small
    , size
    )


normal : Board
normal =
    Board 19


small : Board
small =
    Board 13


beginner : Board
beginner =
    Board 9


type Board
    = Board Int


size : Board -> Int
size board =
    case board of
        Board x ->
            x
