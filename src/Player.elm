module Player exposing (Player(..), name)


type Player
    = Black
    | White


name : Player -> String
name p =
    case p of
        Black ->
            "Black"

        White ->
            "White"
