module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame, onKeyDown, onKeyPress, onKeyUp)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
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


enemyRespawn : { x : Float, y : Float }
enemyRespawn =
    { x = gameWidth / 2 - boxWidth / 2
    , y = gameWidth / 2 - boxWidth / 2
    }


gameMap : List { x : Float, y : Float, char : Char }
gameMap =
    [ "# ################ #"
    , "#                  #"
    , "# ### ######## ### #"
    , "# #f            f# #"
    , "# # #### ## #### # #"
    , "#   #f        f#   #"
    , "# ### ###  ### ### #"
    , "   f#f        f#f   "
    , "# # ### #ii# ### # #"
    , "#       #ii#       #"
    , "# ### # #  # # ### #"
    , "#f    #f    f#    f#"
    , "##### ###  ### #####"
    , "     f#f    f#f     "
    , "# # ### #### ### # #"
    , "# #f            f# #"
    , "# ###### ## ###### #"
    , "#   ##f  ##  f##   #"
    , "# #f   #f  f#   f# #"
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


initialFood : List { x : Float, y : Float }
initialFood =
    List.filter (.char >> (==) 'f') gameMap
        |> List.map (\{ x, y } -> { x = x, y = y })


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
    , bullets : List Bullet
    , food : List Food
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
    , path : List ( Float, Float )
    , isLockedOn : Bool
    }


type alias Bullet =
    { x : Float
    , y : Float
    , direction : Direction
    }


type alias Food =
    { x : Float
    , y : Float
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
            [ { x = boxWidth * 8, y = boxWidth * 7, direction = Up, changeDirectionTime = 0, path = [], isLockedOn = True }
            , { x = boxWidth * 11, y = boxWidth * 7, direction = Up, changeDirectionTime = 0, path = [], isLockedOn = False }
            , { x = boxWidth * 8, y = boxWidth * 11, direction = Up, changeDirectionTime = 0, path = [], isLockedOn = True }
            , { x = boxWidth * 11, y = boxWidth * 11, direction = Up, changeDirectionTime = 0, path = [], isLockedOn = False }
            ]
      , bullets = []
      , food = initialFood
      }
    , Cmd.none
    )


type Msg
    = Restart
    | AnimationFrame Int
    | KeyDown String
    | KeyUp String
    | KeyPress String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart ->
            init ()

        AnimationFrame timeNow ->
            ( { model
                | player = updatePlayer (playerDirection model.keysDown) model.player
                , enemies = updateEnemies timeNow model.player model.bullets model.enemies
                , bullets = updateBullets model.enemies model.bullets
                , food = updateFood model.player model.food
              }
            , Cmd.none
            )

        KeyDown key ->
            ( { model | keysDown = Set.insert key model.keysDown }, Cmd.none )

        KeyUp key ->
            ( { model | keysDown = Set.remove key model.keysDown }, Cmd.none )

        KeyPress key ->
            case bulletDirection key |> Maybe.map (Bullet model.player.x model.player.y) of
                Nothing ->
                    ( model, Cmd.none )

                Just bullet ->
                    ( { model | bullets = bullet :: model.bullets }, Cmd.none )


bulletDirection : String -> Maybe Direction
bulletDirection key =
    case key of
        "w" ->
            Just Up

        "s" ->
            Just Down

        "a" ->
            Just Left

        "d" ->
            Just Right

        _ ->
            Nothing


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


updateEnemies : Int -> Player -> List Bullet -> List Enemy -> List Enemy
updateEnemies timeNow player bullets enemies =
    List.map (updateEnemy timeNow player bullets) enemies


updateEnemy : Int -> Player -> List Bullet -> Enemy -> Enemy
updateEnemy timeNow player bullets enemy =
    let
        hitByAnyBullets =
            List.any (overlapping enemy) bullets

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

        path =
            case enemy.path of
                [] ->
                    shortestPath ( enemy.x, enemy.y ) ( player.x, player.y )

                ( nextX, nextY ) :: remPath ->
                    if enemy.x == nextX && enemy.y == nextY then
                        remPath

                    else
                        enemy.path

        ( pathX, pathY ) =
            List.head path |> Maybe.withDefault ( enemy.x, enemy.y )

        ( pathDx, pathDy ) =
            ( clamp -moveSpeed moveSpeed <| pathX - enemy.x
            , clamp -moveSpeed moveSpeed <| pathY - enemy.y
            )
    in
    if hitByAnyBullets then
        { enemy | x = enemyRespawn.x, y = enemyRespawn.y, path = [] }

    else if enemy.isLockedOn then
        { enemy | x = enemy.x + pathDx, y = enemy.y + pathDy, path = path }

    else if not isOldDirectionOverlappingWalls && timeNow < enemy.changeDirectionTime then
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


updateBullets : List Enemy -> List Bullet -> List Bullet
updateBullets enemies bullets =
    bullets
        |> List.filter
            (\bullet ->
                not (List.any (overlapping bullet) enemies)
                    && not (List.any (overlapping bullet) gameMapWalls)
                    && (bullet.x >= 0 && bullet.x < gameWidth)
                    && (bullet.y >= 0 && bullet.y < gameWidth)
            )
        |> List.map
            (\bullet ->
                let
                    ( dx, dy ) =
                        directionDeltas bullet.direction
                in
                { bullet | x = bullet.x + dx, y = bullet.y + dy }
            )


updateFood : Player -> List Food -> List Food
updateFood player food =
    List.filter (not << overlapping player) food


shortestPath : ( Float, Float ) -> ( Float, Float ) -> List ( Float, Float )
shortestPath source destination =
    shortestPath_ Set.empty (queueEmpty |> queueAdd [ Tuple.mapBoth normalize normalize source ]) destination


shortestPath_ : Set ( Float, Float ) -> Queue (List ( Float, Float )) -> ( Float, Float ) -> List ( Float, Float )
shortestPath_ visited queue destination =
    case queueNext queue of
        ( Nothing, _ ) ->
            []

        ( Just [], newQueue ) ->
            shortestPath_ visited newQueue destination

        ( Just ((( x, y ) :: _) as path), newQueue ) ->
            if ( x, y ) == destination then
                List.reverse path

            else
                [ ( 0, 1 ), ( 0, -1 ), ( -1, 0 ), ( 1, 0 ) ]
                    |> List.map (\( dx, dy ) -> ( x + dx * boxWidth, y + dy * boxWidth ))
                    |> List.filter
                        (\( newX, newY ) ->
                            not (Set.member ( newX, newY ) visited)
                                && not (List.any (\wall -> wall.x == newX && wall.y == newY) gameMapWalls)
                                && (newX >= 0 && newX + boxWidth < gameWidth)
                                && (newY >= 0 && newY + boxWidth < gameWidth)
                        )
                    |> List.foldl
                        (\( newX, newY ) ( newVisited, newQueue_ ) ->
                            ( Set.insert ( newX, newY ) newVisited
                            , queueAdd (( newX, newY ) :: path) newQueue_
                            )
                        )
                        ( visited, newQueue )
                    |> (\( newVisited, newQueue_ ) -> shortestPath_ newVisited newQueue_ destination)


normalize : Float -> Float
normalize x =
    toFloat (round x // round boxWidth) * boxWidth


wrapAround : Float -> Float
wrapAround x =
    x |> round |> modBy (round gameWidth) |> toFloat


overlapping : { r | x : Float, y : Float } -> { r2 | x : Float, y : Float } -> Bool
overlapping p1 p2 =
    (p1.x < p2.x + boxWidth && p1.x + boxWidth > p2.x)
        && (p1.y < p2.y + boxWidth && p1.y + boxWidth > p2.y)


type Queue a
    = Queue (List a) (List a)


queueEmpty : Queue a
queueEmpty =
    Queue [] []


queueAdd : a -> Queue a -> Queue a
queueAdd newest (Queue oldestToNewer newestToOlder) =
    Queue oldestToNewer (newest :: newestToOlder)


queueNext : Queue a -> ( Maybe a, Queue a )
queueNext queue =
    case queue of
        Queue [] [] ->
            ( Nothing, Queue [] [] )

        Queue [] newestToOlder ->
            queueNext (Queue (List.reverse newestToOlder) [])

        Queue (oldest :: oldestToNewer) newestToOlder ->
            ( Just oldest, Queue oldestToNewer newestToOlder )


view : Model -> Html Msg
view model =
    div []
        [ div
            [ style "position" "relative"
            , style "border" "1px solid cyan"
            , style "width" (px gameWidth)
            , style "height" (px gameWidth)
            , style "margin" "40px auto"
            , style "overflow" "hidden"
            ]
            [ viewGameMap
            , viewPlayer model.player
            , viewFood model.food
            , viewEnemies model.enemies
            , viewBullets model.bullets
            ]
        , viewBottom model
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


viewBullets : List Bullet -> Html msg
viewBullets bullets =
    div [] (List.map (\{ x, y } -> viewBox { x = x, y = y, color = "cyan" }) bullets)


viewFood : List Food -> Html msg
viewFood food =
    div [] (List.map (\{ x, y } -> viewBox { x = x, y = y, color = "white" }) food)


viewBottom : Model -> Html Msg
viewBottom model =
    if model.food == [] then
        viewGameOverMessage True

    else if List.any (overlapping model.player) model.enemies then
        viewGameOverMessage False

    else
        viewGameControls


viewGameOverMessage : Bool -> Html Msg
viewGameOverMessage isWin =
    div []
        [ div
            [ style "font-size" "20px"
            , style "font-weight" "bold"
            , style "text-align" "center"
            , style "margin-bottom" "10px"
            , style "color" <|
                if isWin then
                    "white"

                else
                    "red"
            ]
            [ text <|
                if isWin then
                    "You win!"

                else
                    "You lose :("
            ]
        , button
            [ style "display" "block"
            , style "margin" "0 auto"
            , onClick Restart
            ]
            [ text "Restart" ]
        ]


viewGameControls : Html Msg
viewGameControls =
    div
        [ style "position" "relative"
        , style "left" "0"
        , style "right" "0"
        , style "width" "400px"
        , style "height" "50px"
        , style "margin" "0 auto"
        ]
        [ -- Left side
          button
            [ style "position" "absolute"
            , style "left" "50px"
            , style "top" "50px"
            , onTouchStart (KeyDown "a")
            , onTouchEnd (KeyUp "a")
            ]
            [ text "<" ]
        , button
            [ style "position" "absolute"
            , style "left" "100px"
            , style "top" "50px"
            , onTouchStart (KeyDown "d")
            , onTouchEnd (KeyUp "d")
            ]
            [ text ">" ]
        , button
            [ style "position" "absolute"
            , style "left" "75px"
            , style "top" "25px"
            , onTouchStart (KeyDown "w")
            , onTouchEnd (KeyUp "w")
            ]
            [ text "^" ]
        , button
            [ style "position" "absolute"
            , style "left" "75px"
            , style "top" "75px"
            , onTouchStart (KeyDown "s")
            , onTouchEnd (KeyUp "s")
            ]
            [ text "v" ]

        -- Right side
        , button
            [ style "position" "absolute"
            , style "right" "100px"
            , style "top" "50px"
            , onTouchStart (KeyDown "ArrowLeft")
            , onTouchEnd (KeyUp "ArrowLeft")
            ]
            [ text "<" ]
        , button
            [ style "position" "absolute"
            , style "right" "50px"
            , style "top" "50px"
            , onTouchStart (KeyDown "ArrowRight")
            , onTouchEnd (KeyUp "ArrowRight")
            ]
            [ text ">" ]
        , button
            [ style "position" "absolute"
            , style "right" "75px"
            , style "top" "25px"
            , onTouchStart (KeyDown "ArrowUp")
            , onTouchEnd (KeyUp "ArrowUp")
            ]
            [ text "^" ]
        , button
            [ style "position" "absolute"
            , style "right" "75px"
            , style "top" "75px"
            , onTouchStart (KeyDown "ArrowDown")
            , onTouchEnd (KeyUp "ArrowDown")
            ]
            [ text "v" ]
        ]


onTouchStart : msg -> Html.Attribute msg
onTouchStart msg =
    Html.Events.on "touchstart" (Decode.succeed msg)


onTouchEnd : msg -> Html.Attribute msg
onTouchEnd msg =
    Html.Events.on "touchend" (Decode.succeed msg)


px : Float -> String
px pixels =
    String.fromFloat pixels ++ "px"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.food == [] || List.any (overlapping model.player) model.enemies then
            Sub.none

          else
            onAnimationFrame (Time.posixToMillis >> AnimationFrame)
        , onKeyDown (Decode.map KeyDown (Decode.field "key" Decode.string))
        , onKeyUp (Decode.map KeyUp (Decode.field "key" Decode.string))
        , onKeyPress (Decode.map KeyPress (Decode.field "key" Decode.string))
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
