module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyUp)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Json.Decode as Decode
import Set exposing (Set)
import Time


gameWidth : Float
gameWidth =
    400


boxWidth : Float
boxWidth =
    gameWidth / 20


moveSpeed : Float
moveSpeed =
    2


changeDirectionTime : Int
changeDirectionTime =
    500


gameMap : List { x : Float, y : Float, char : Char }
gameMap =
    [ "# ################ #"
    , "#                  #"
    , "# ### ######## ### #"
    , "# #              # #"
    , "# # #### ## #### # #"
    , "#   #          #   #"
    , "# ### ###  ### ### #"
    , "    #          #    "
    , "# # ### #ii# ### # #"
    , "#       #ii#       #"
    , "# ### # #  # # ### #"
    , "#     #      #     #"
    , "##### ###  ### #####"
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
                        , char = gameMapChar
                        }
                    )
                    row
            )
        |> List.concat


gameMapWalls : List { x : Float, y : Float, char : Char }
gameMapWalls =
    List.filter (.char >> (==) '#') gameMap


gameMapCharToColor : Char -> String
gameMapCharToColor gameMapChar =
    case gameMapChar of
        '#' ->
            "blue"

        'i' ->
            "#ff00ff"

        _ ->
            "black"


type alias Model =
    { player : Player
    , keysDown : Set String
    , enemies : List Enemy
    }


type alias Player =
    { x : Float
    , y : Float
    , direction : Direction
    }


type alias Enemy =
    { x : Float
    , y : Float
    , direction : Direction
    , changeDirectionTime : Int
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
      , enemies =
            [ { x = boxWidth * 8, y = boxWidth * 7, direction = Up, changeDirectionTime = 0 }
            , { x = boxWidth * 11, y = boxWidth * 7, direction = Up, changeDirectionTime = 0 }
            , { x = boxWidth * 8, y = boxWidth * 11, direction = Up, changeDirectionTime = 0 }
            , { x = boxWidth * 11, y = boxWidth * 11, direction = Up, changeDirectionTime = 0 }
            ]
      }
    , Cmd.none
    )


type Msg
    = NoOp
    | AnimationFrame Int
    | KeyDown String
    | KeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        AnimationFrame timeNow ->
            ( { model
                | player = updatePlayer (playerDirection model.keysDown) model.player
                , enemies = updateEnemies timeNow model.enemies
              }
            , Cmd.none
            )

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
    (case direction of
        Up ->
            ( 0, -1 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )

        Right ->
            ( 1, 0 )
    )
        |> Tuple.mapBoth ((*) moveSpeed) ((*) moveSpeed)


updatePlayer : Maybe Direction -> Player -> Player
updatePlayer maybeNewDirection player =
    let
        newDirection =
            Maybe.withDefault player.direction maybeNewDirection

        ( newDx, newDy ) =
            directionDeltas newDirection

        ( newX, newY ) =
            ( player.x + newDx, player.y + newDy )
                |> Tuple.mapBoth wrapAround wrapAround

        isNewDirectionOverlappingWalls =
            List.any (overlapping { x = newX, y = newY }) gameMapWalls

        ( oldDx, oldDy ) =
            directionDeltas player.direction

        ( oldX, oldY ) =
            ( player.x + oldDx, player.y + oldDy )
                |> Tuple.mapBoth wrapAround wrapAround

        isOldDirectionOverlappingWalls =
            List.any (overlapping { x = oldX, y = oldY }) gameMapWalls
    in
    if not isNewDirectionOverlappingWalls then
        { player | x = newX, y = newY, direction = newDirection }

    else if not isOldDirectionOverlappingWalls then
        { player | x = oldX, y = oldY }

    else
        player


updateEnemies : Int -> List Enemy -> List Enemy
updateEnemies timeNow enemies =
    List.map (updateEnemy timeNow) enemies


updateEnemy : Int -> Enemy -> Enemy
updateEnemy timeNow enemy =
    let
        newDirection =
            case timeNow |> modBy 4 of
                0 ->
                    Up

                1 ->
                    Down

                2 ->
                    Left

                _ ->
                    Right

        ( newDx, newDy ) =
            directionDeltas newDirection

        ( newX, newY ) =
            ( enemy.x + newDx, enemy.y + newDy )
                |> Tuple.mapBoth wrapAround wrapAround

        isNewDirectionOverlappingWalls =
            List.any (overlapping { x = newX, y = newY }) gameMapWalls

        ( oldDx, oldDy ) =
            directionDeltas enemy.direction

        ( oldX, oldY ) =
            ( enemy.x + oldDx, enemy.y + oldDy )
                |> Tuple.mapBoth wrapAround wrapAround

        isOldDirectionOverlappingWalls =
            List.any (overlapping { x = oldX, y = oldY }) gameMapWalls
    in
    if not isOldDirectionOverlappingWalls && timeNow < enemy.changeDirectionTime then
        { enemy | x = oldX, y = oldY }

    else if not isNewDirectionOverlappingWalls then
        { enemy
            | x = newX
            , y = newY
            , direction = newDirection
            , changeDirectionTime = timeNow + changeDirectionTime
        }

    else
        enemy


wrapAround : Float -> Float
wrapAround x =
    x |> round |> modBy (round gameWidth) |> toFloat


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
        , style "overflow" "hidden"
        ]
        [ viewGameMap
        , viewPlayer model.player
        , viewEnemies model.enemies
        ]


viewGameMap : Html msg
viewGameMap =
    div []
        (gameMap
            |> List.map (\r -> { x = r.x, y = r.y, color = gameMapCharToColor r.char })
            |> List.map viewBox
        )


viewPlayer : Player -> Html msg
viewPlayer player =
    div []
        [ viewBox { x = player.x, y = player.y, color = "yellow" }
        , if player.x + boxWidth > gameWidth then
            viewBox
                { x = wrapAround (player.x + boxWidth) - boxWidth, y = player.y, color = "yellow" }

          else if player.y + boxWidth > gameWidth then
            viewBox
                { x = player.x, y = wrapAround (player.y + boxWidth) - boxWidth, color = "yellow" }

          else
            text ""
        ]


viewEnemies : List Enemy -> Html msg
viewEnemies enemies =
    div [] (List.map (\enemy -> viewBox { x = enemy.x, y = enemy.y, color = "red" }) enemies)


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
        [ onAnimationFrame (Time.posixToMillis >> AnimationFrame)
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
