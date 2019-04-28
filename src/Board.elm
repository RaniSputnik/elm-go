module Board exposing
    ( Board
    , IllegalMove
    , beginner
    , explain
    , normal
    , play
    , pos
    , size
    , small
    , turn
    )

import Array exposing (Array)
import Player exposing (Player(..))


type Board
    = Board Int (Array (Maybe Player))


type IllegalMove
    = PositionOccupied Int Int


explain : IllegalMove -> String
explain err =
    case err of
        PositionOccupied x y ->
            "A stone has already been placed at position: (" ++ String.fromInt x ++ "," ++ String.fromInt y ++ ")"


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


pos : Int -> Int -> Board -> Maybe Player
pos x y board =
    let
        i =
            indexOf x y board
    in
    Maybe.withDefault Nothing (Array.get i (positions board))


turn : Board -> Player
turn board =
    let
        hasStone =
            \p ->
                case p of
                    Nothing ->
                        False

                    Just _ ->
                        True

        numberOfStones =
            Array.length (Array.filter hasStone (positions board))

        isEven =
            \x ->
                modBy 2 x == 0
    in
    if isEven numberOfStones then
        White

    else
        Black


play : Int -> Int -> Board -> Result IllegalMove Board
play x y board =
    let
        nextPlayer =
            turn board

        playedAt =
            indexOf x y board

        currentBoardState =
            pos x y board

        nextBoardState =
            Array.set playedAt (Just nextPlayer) (positions board)
    in
    case currentBoardState of
        Nothing ->
            Ok (Board (size board) nextBoardState)

        Just _ ->
            Err (PositionOccupied x y)



-- Internal


new : Int -> Board
new x =
    Board x (Array.repeat (x * x) Nothing)


positions : Board -> Array (Maybe Player)
positions board =
    case board of
        Board _ p ->
            p


indexOf : Int -> Int -> Board -> Int
indexOf x y board =
    x + y * size board
