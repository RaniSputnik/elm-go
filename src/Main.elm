module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Board exposing (Board)
import Browser
import Html exposing (Html, div, pre, table, tbody, td, text, tr)
import Http
import Stone exposing (Stone(..))



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { board : Board
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = Board.beginner
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    viewBoard model.board


viewBoard : Board -> Html Msg
viewBoard board =
    let
        cells =
            List.range 0 (Board.size board - 1)
    in
    table []
        [ tbody []
            (List.map (\y -> tr [] (List.map (\x -> viewBoardCell x y board) cells)) cells)
        ]


viewBoardCell : Int -> Int -> Board -> Html Msg
viewBoardCell x y board =
    let
        empty =
            "  +  "

        black =
            "  O  "

        white =
            "  X  "

        theStone =
            case Board.pos x y board of
                Nothing ->
                    empty

                Just Black ->
                    black

                Just White ->
                    white
    in
    td [] [ pre [] [ text theStone ] ]
