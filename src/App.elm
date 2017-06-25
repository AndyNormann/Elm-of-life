module App exposing (..)

import Html exposing (Html, div, text, h1, h5, p, button, span)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Debug exposing (log)
import Matrix exposing (square, flatten, mapWithLocation, Matrix)
import Element exposing (toHtml)
import Collage exposing (filled, rect, move, collage)
import Color exposing (rgb)
import Mouse
import Keyboard
import Time exposing (Time, millisecond)


( square_size, square_amount ) =
    ( 20, 30 )


collage_size : Int
collage_size =
    (square_size * square_amount)


type alias Grid =
    Matrix SquareStatus


type alias Model =
    { grid : Grid
    , running : Bool
    , key : Keyboard.KeyCode
    , timestep : Time
    }



-- Init starts with a glider as an example


init : ( Model, Cmd msg )
init =
    ( { grid =
            (Matrix.square square_amount (\loc -> Dead)
                |> Matrix.set ( 3, 21 ) Alive
                |> Matrix.set ( 4, 21 ) Alive
                |> Matrix.set ( 5, 21 ) Alive
                |> Matrix.set ( 5, 22 ) Alive
                |> Matrix.set ( 4, 23 ) Alive
            )
      , running = True
      , key = 0
      , timestep = 100
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick
    | Pause
    | MouseMsg Mouse.Position
    | KeyMsg Keyboard.KeyCode
    | Inc
    | Dec
    | TimeTick Time
    | Clear


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        Tick ->
            { model | grid = (advance model.grid) } ! []

        TimeTick time ->
            if model.running then
                { model | grid = (advance model.grid) }
                    ! []
            else
                model ! []

        Inc ->
            { model | timestep = model.timestep + 20 } ! []

        Dec ->
            { model | timestep = model.timestep - 20 } ! []

        Pause ->
            { model | running = not model.running } ! []

        MouseMsg position ->
            model ! []

        KeyMsg code ->
            { model | key = code } ! []

        Clear ->
            init


advance : Grid -> Grid
advance grid =
    mapWithLocation
        (\loc value ->
            let
                ( x, y ) =
                    loc

                neighs =
                    neighbours grid ( x, y )
            in
                if neighs < 2 || neighs > 3 then
                    Dead
                else if neighs == 3 then
                    Alive
                else
                    value
        )
        grid


neighbours : Grid -> Matrix.Location -> Int
neighbours grid loc =
    let
        ( x, y ) =
            loc

        locations =
            [ ( -1, 0 ), ( 1, 0 ), ( -1, -1 ), ( 0, -1 ), ( 1, -1 ), ( -1, 1 ), ( 0, 1 ), ( 1, 1 ) ]
    in
        List.sum
            (List.map
                (\e ->
                    let
                        ( xOff, yOff ) =
                            e
                    in
                        case Matrix.get ( x + xOff, y + yOff ) grid of
                            Just x ->
                                if x == Alive then
                                    1
                                else
                                    0

                            Nothing ->
                                0
                )
                locations
            )



-- VIEW


type SquareStatus
    = Alive
    | Dead


square : Float -> Float -> SquareStatus -> Collage.Form
square x y alive =
    let
        color =
            case alive of
                Alive ->
                    (rgb 0 0 0)

                Dead ->
                    (rgb 254 254 254)

        _ =
            log ("Halla " ++ (toString color))
    in
        rect square_size square_size
            |> filled color
            |> move ( x, y )


draw : ( Int, Int ) -> SquareStatus -> Collage.Form
draw loc value =
    let
        ( xPos, yPos ) =
            loc

        offset =
            (square_size * square_amount) / 2

        x =
            ((toFloat xPos) * square_size) - offset

        y =
            ((toFloat yPos) * square_size) - offset
    in
        square x y value


drawMatrix : Matrix SquareStatus -> List Collage.Form
drawMatrix mat =
    Matrix.mapWithLocation draw mat
        |> Matrix.flatten


view : Model -> Html Msg
view model =
    div [ class "center" ]
        [ div []
            [ h1 [ class "center" ] [ text "Game of Life" ]
            , div []
                [ drawMatrix model.grid
                    |> collage collage_size collage_size
                    |> toHtml
                ]
            , div [ class "center" ]
                [ button [ class "btn btn-info", onClick Tick ] [ text "Step" ]
                , button [ class "btn btn-primary", onClick Pause ]
                    [ if model.running == True then
                        text "Pause"
                      else
                        text "Unpause"
                    ]
                , button [ class "btn btn-info", onClick Clear ] [ text "Clear" ]
                , button [ class "btn btn-secondary", onClick Dec ] [ text "^" ]
                , button [ class "btn btn-secondary", onClick Inc ] [ text "v" ]
                ]
            ]
        ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks MouseMsg
        , Time.every (model.timestep * millisecond) TimeTick
        , Keyboard.downs KeyMsg
        ]
