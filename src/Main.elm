module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyUp)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Set exposing (Set)


gameWidth : Float
gameWidth =
    400


boxWidth : Float
boxWidth =
    gameWidth / 20


gameMap : List { x : Float, y : Float, color : String, char : Char }
gameMap =
    [ "# ################ #"
    , "#                  #"
    , "# ### ######## ### #"
    , "# #              # #"
    , "# # #### ## #### # #"
    , "#   #    ##    #   #"
    , "# ### ######## ### #"
    , "                    "
    , "# ##### #### ##### #"
    , "#       #  #       #"
    , "# ### # #### # ### #"
    , "#     #      #     #"
    , "##### ######## #####"
    , "      #      #      "
    , "# # ### #### ### # #"
    , "# #              # #"
    , "# ###### ## ###### #"
    , "#   ##   ##   ##   #"
    , "# #    #    #    # #"
    , "# ################ #"
    ]
        |> List.map String.toList
        |> List.indexedMap
            (\y row ->
                List.indexedMap
                    (\x gameMapChar ->
                        { x = toFloat x * boxWidth
                        , y = toFloat y * boxWidth
                        , color = gameMapCharToColor gameMapChar
                        , char = gameMapChar
                        }
                    )
                    row
            )
        |> List.concat


gameMapWalls : List { x : Float, y : Float, color : String, char : Char }
gameMapWalls =
    List.filter (.color >> (==) "blue") gameMap


gameMapCharToColor : Char -> String
gameMapCharToColor gameMapChar =
    case gameMapChar of
        '#' ->
            "blue"

        _ ->
            "black"


type alias Model =
    { player : Player
    , keysDown : Set String
    }


type alias Player =
    { x : Float
    , y : Float
    , direction : Direction
    }


type Direction
    = Up
    | Down
    | Left
    | Right


init : () -> ( Model, Cmd Msg )
init _ =
    ( { player = { x = boxWidth * 9.5, y = boxWidth * 15, direction = Left }
      , keysDown = Set.empty
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | AnimationFrame
    | KeyDown String
    | KeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AnimationFrame ->
            ( { model | player = updatePlayer (playerDirection model.keysDown) model.player }, Cmd.none )

        KeyDown key ->
            ( { model | keysDown = Set.insert key model.keysDown }, Cmd.none )

        KeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }, Cmd.none )


playerDirection : Set String -> Maybe Direction
playerDirection keysDown =
    if Set.member "ArrowUp" keysDown then
        Just Up

    else if Set.member "ArrowDown" keysDown then
        Just Down

    else if Set.member "ArrowLeft" keysDown then
        Just Left

    else if Set.member "ArrowRight" keysDown then
        Just Right

    else
        Nothing


directionDeltas : Direction -> ( Float, Float )
directionDeltas direction =
    case direction of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )


updatePlayer : Maybe Direction -> Player -> Player
updatePlayer maybeNewDirection player =
    let
        newDirection =
            Maybe.withDefault player.direction maybeNewDirection

        ( newDx, newDy ) =
            directionDeltas newDirection

        ( newX, newY ) =
            ( player.x + newDx, player.y + newDy )

        isNewDirectionOverlappingWalls =
            List.any (overlapping { x = newX, y = newY }) gameMapWalls

        ( oldDx, oldDy ) =
            directionDeltas player.direction

        ( oldX, oldY ) =
            ( player.x + oldDx, player.y + oldDy )

        isOldDirectionOverlappingWalls =
            List.any (overlapping { x = oldX, y = oldY }) gameMapWalls
    in
    if not isNewDirectionOverlappingWalls then
        { player | x = newX, y = newY, direction = newDirection }

    else if not isOldDirectionOverlappingWalls then
        { player | x = oldX, y = oldY }

    else
        player


overlapping : { r | x : Float, y : Float } -> { r2 | x : Float, y : Float } -> Bool
overlapping p1 p2 =
    (p1.x < p2.x + boxWidth && p1.x + boxWidth > p2.x)
        && (p1.y < p2.y + boxWidth && p1.y + boxWidth > p2.y)


view : Model -> Html Msg
view model =
    div
        [ style "position" "relative"
        , style "border" "1px solid cyan"
        , style "width" (px gameWidth)
        , style "height" (px gameWidth)
        , style "margin" "40px auto"
        ]
        [ viewGameMap
        , viewBox { x = model.player.x, y = model.player.y, color = "yellow" }
        ]


viewGameMap : Html msg
viewGameMap =
    div [] (List.map viewBox gameMap)


viewBox : { r | x : Float, y : Float, color : String } -> Html msg
viewBox { x, y, color } =
    div
        [ style "position" "absolute"
        , style "left" (px x)
        , style "top" (px y)
        , style "width" (px boxWidth)
        , style "height" (px boxWidth)
        , style "background-color" color
        ]
        []


px : Float -> String
px pixels =
    String.fromFloat pixels ++ "px"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrame (\_ -> AnimationFrame)
        , onKeyDown (Decode.map KeyDown (Decode.field "key" Decode.string))
        , onKeyUp (Decode.map KeyUp (Decode.field "key" Decode.string))
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
