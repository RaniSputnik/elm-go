module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Board exposing (Board)
import Browser
import Html exposing (Html, div, pre, table, tbody, td, text, tr)
import Http



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
            List.repeat (Board.size board) 0
    in
    table []
        [ tbody []
            (List.map (\_ -> tr [] (List.map (\_ -> viewBoardCell) cells)) cells)
        ]


viewBoardCell : Html Msg
viewBoardCell =
    td [] [ pre [] [ text "  +  " ] ]
