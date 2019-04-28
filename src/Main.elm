module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Board exposing (Board, IllegalMove)
import Browser
import Html exposing (Html, div, span, table, tbody, td, text, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Player exposing (Player(..))



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
    , error : Maybe IllegalMove
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = Board.beginner
      , error = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Noop
    | Play Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        Play x y ->
            let
                playAttempt =
                    Board.play x y model.board
            in
            case playAttempt of
                Ok nextBoardState ->
                    ( { model | board = nextBoardState, error = Nothing }, Cmd.none )

                Err mv ->
                    ( { model | error = Just mv }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text ("Next move: " ++ Player.name (Board.turn model.board))
        , viewBoard model.board
        , viewError model.error
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    let
        cells =
            List.range 0 (Board.size board - 1)
    in
    table [ class "ok" ]
        [ tbody []
            (List.map (\y -> tr [] (List.map (\x -> viewBoardCell x y board) cells)) cells)
        ]


viewBoardCell : Int -> Int -> Board -> Html Msg
viewBoardCell x y board =
    let
        thePlayer =
            case Board.pos x y board of
                Nothing ->
                    ""

                Just Black ->
                    "b"

                Just White ->
                    "w"
    in
    td [ onClick (Play x y) ] [ div [ class "s", class thePlayer ] [] ]


viewError : Maybe IllegalMove -> Html Msg
viewError maybeErr =
    case maybeErr of
        Nothing ->
            text ""

        Just err ->
            text (Board.explain err)
