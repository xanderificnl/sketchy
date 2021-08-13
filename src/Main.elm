module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseOver)
import Html.Events.Extra.Wheel as Wheel



-- Some green: red = 25, green = 200, blue = 110


type Msg
    = ColorCell Int
    | ChangeColor Wheel.Event


type alias RGB =
    { red : Int, green : Int, blue : Int }


type alias Cell =
    { id : Int, color : RGB }


type alias Model =
    { color : RGB, previousColor : RGB, cells : List Cell, colors : List RGB }


colorList : List RGB
colorList =
    [ { red = 50, green = 50, blue = 50 } -- gray
    , { red = 252, green = 186, blue = 3 } -- orange
    , { red = 252, green = 231, blue = 3 } -- yellow
    , { red = 1, green = 99, blue = 255 } -- blue
    , { red = 25, green = 200, blue = 110 } -- green
    , { red = 200, green = 0, blue = 50 } -- red
    , { red = 255, green = 255, blue = 255 } -- white
    ]


initialModel : Model
initialModel =
    { color =
        { red = 50, green = 50, blue = 50 }
    , previousColor = { red = 255, green = 255, blue = 255 }
    , cells = createCells (24 ^ 2) []
    , colors = colorList
    }


view : Model -> Html Msg
view model =
    div
        [ id "grid"
        , style "border-color" (getRgb model.color)
        , Wheel.onWheel ChangeColor
        ]
        (List.map viewCell model.cells)


getRgb : RGB -> String
getRgb color =
    "rgb("
        ++ String.join "," [ String.fromInt (.red color), String.fromInt (.green color), String.fromInt (.blue color) ]
        ++ ")"


viewCell : Cell -> Html Msg
viewCell cell =
    div
        [ style "background-color" (getRgb cell.color)
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
        ChangeColor wheel ->
            let
                findNext n =
                    case n of
                        [] ->
                            Nothing

                        x :: [] ->
                            if x == model.color then
                                List.head model.colors

                            else
                                Nothing

                        x :: y :: rest ->
                            if x == model.color then
                                Just y

                            else
                                findNext (y :: rest)

                listOfColors =
                    if wheel.deltaY > 0 then
                        List.reverse model.colors

                    else
                        model.colors

                newColor =
                    if wheel.deltaY > 0 then
                        model.previousColor

                    else
                        Maybe.withDefault model.color (findNext model.colors)
            in
            { model | color = newColor, previousColor = model.color }

        --List.map (\color ->
        --    if color = model.color then
        --
        --)
        --in
        --{ model | color = { red = 1, green = 99, blue = 255 } }
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
