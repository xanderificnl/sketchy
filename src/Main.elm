module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onMouseOver)
import Html.Events.Extra.Wheel as Wheel
import Json.Decode as Decode


type Msg
    = ColorCell Int
    | ChangeColor Wheel.Event
    | KeyPressDowns String -- ?
    | ClearPressed


type alias RGB =
    { red : Int, green : Int, blue : Int }


type alias Cell =
    { id : Int, color : RGB }


type alias Model =
    { color : RGB, previousColor : RGB, cells : List Cell, colors : List RGB }


getColor : Int -> RGB
getColor n =
    case n of
        0 ->
            -- white
            { red = 255, green = 255, blue = 255 }

        {--*** this value only exists to make it clear we're using our catch-all. *** --}
        1 ->
            getColor 999

        2 ->
            --  yellow
            { red = 252, green = 231, blue = 3 }

        3 ->
            -- blue
            { red = 1, green = 99, blue = 255 }

        4 ->
            -- green
            { red = 25, green = 200, blue = 110 }

        5 ->
            -- red
            { red = 200, green = 0, blue = 50 }

        6 ->
            -- orange
            { red = 252, green = 186, blue = 3 }

        _ ->
            { red = 50, green = 50, blue = 50 }


colorList : List RGB
colorList =
    List.map getColor (List.range 0 6)


initialModel : Model
initialModel =
    { color = getColor 999
    , previousColor = getColor 0
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
            { id = n, color = getColor 0 } :: cells
    in
    case n of
        1 ->
            newCells

        _ ->
            createCells (n - 1) newCells


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyPressDowns code ->
            ( { model | color = getColor (Maybe.withDefault 0 (String.toInt code)) }, Cmd.none )

        ClearPressed ->
            ( model, Cmd.none )

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
            ( { model | color = newColor, previousColor = model.color }, Cmd.none )

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
            ( { model | cells = newCells }, Cmd.none )


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map KeyPressDowns keyDecoder)
        , onKeyUp (Decode.succeed ClearPressed)
        ]


main : Program () Model Msg
main =
    Browser.document
        { init = \() -> ( initialModel, Cmd.none )
        , update = update
        , view =
            \m ->
                { title = "Sketchin' app"
                , body = [ view m ]
                }
        , subscriptions = subscriptions
        }
