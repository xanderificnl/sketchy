module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseOver)
import Json.Encode


type Msg
    = ColorCell Int


type alias RGB =
    { red : Int, green : Int, blue : Int }


type alias Cell =
    { id : Int, color : RGB }


type alias Model =
    { color : RGB, cells : List Cell }


initialModel : Model
initialModel =
    { color =
        { red = 50, green = 50, blue = 50 }
    , cells = createCells (24 * 24) []
    }


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "pure-g" ] (List.map viewCell model.cells)
        ]



--(List.map viewCell model.cells)


viewCell : Cell -> Html Msg
viewCell cell =
    let
        rgbStyle =
            "rgb("
                ++ String.fromInt (.red cell.color)
                ++ ","
                ++ String.fromInt (.green cell.color)
                ++ ","
                ++ String.fromInt (.blue cell.color)
                ++ ")"
    in
    div
        [ class "pure-u-1-24 cell"
        , style "background-color" rgbStyle
        , onMouseOver (ColorCell cell.id)
        ]
        [ text "" ]


createCells n cells =
    let
        newCells =
            { id = n, color = { red = 255, green = 255, blue = 255 } } :: cells
    in
    case n of
        1 ->
            newCells

        _ ->
            createCells (n - 1) newCells


update : Msg -> Model -> Model
update msg model =
    case msg of
        ColorCell index ->
            let
                newCells =
                    List.map
                        (\cell ->
                            if cell.id == index then
                                { cell | color = model.color }

                            else
                                cell
                        )
                        model.cells
            in
            { model | cells = newCells }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }



--
--main =
--    text ""
--
--
