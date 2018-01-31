module Ships exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Form.Select as Select
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Table as Table
import Char
import ColorHelper
import Dict exposing (Dict)
import Dict.Extra as DictExtra
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Json
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Task
import Visualization.Scale as Scale
import Window exposing (Size)


type alias Position =
    ( Int, Int )


type alias FieldState =
    { shipId : Maybe ShipId
    , shoot : Bool
    }


type alias Ship =
    { shipId : ShipId
    , length : ShipLength
    , destroyed : Bool
    }


emptyField : FieldState
emptyField =
    { shipId = Nothing, shoot = False }


type alias Model =
    { property : String
    , screenSize : Size
    , gameField : GameField
    , myGameField : GameField
    , ships : Ships
    , myShips : Ships
    , state : GameState
    }


type alias Ships =
    Dict ShipId Ship


type alias GameField =
    Dict Position FieldState


type GameState
    = Normal
    | ShowShips
    | Finished
    | PlaceShips


allGameStates : List GameState
allGameStates =
    [ Normal, ShowShips, Finished, PlaceShips ]


stateFromString : String -> Maybe GameState
stateFromString string =
    case string of
        "Start" ->
            Just Normal

        "ShowShips" ->
            Just ShowShips

        "Finished" ->
            Just Finished

        "PlaceShips" ->
            Just PlaceShips

        _ ->
            Nothing


initModel : Model
initModel =
    { property = "foo"
    , screenSize = { width = 200, height = 200 }
    , gameField = Dict.empty
    , ships = Dict.empty
    , myGameField = Dict.empty
    , myShips = Dict.empty
    , state = Normal
    }


type Msg
    = ScreenSizeChanged Size
    | FieldClicked ( Int, Int )
    | RandomListGenerated (List ( Position, Bool ))
    | StateChanged String
    | Restarted


gameFieldSize : FieldSize
gameFieldSize =
    9


ships : List ( Int, ShipLength )
ships =
    -- Count, ShipLength
    [ ( 4, 2 )
    , ( 3, 3 )
    , ( 2, 4 )
    , ( 1, 5 )
    ]


type alias FieldSize =
    Int


type Direction
    = NS
    | WO


type alias ShipLength =
    Int


type alias ShipId =
    Int


directionFromBool : Bool -> Direction
directionFromBool b =
    if b then
        NS
    else
        WO


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    -- Debug.log "update"
    case msg of
        ScreenSizeChanged size ->
            ( { model | screenSize = size }, Cmd.none )

        FieldClicked gridPoint ->
            fieldClick gridPoint model

        StateChanged stateString ->
            ( { model
                | state = stateFromString stateString |> Maybe.withDefault Normal
              }
            , Cmd.none
            )

        RandomListGenerated rs ->
            buildGameField rs model

        Restarted ->
            init


fieldClick : Position -> Model -> ( Model, Cmd Msg )
fieldClick gridPoint model =
    let
        currentFieldState : FieldState
        currentFieldState =
            model.gameField
                |> Dict.get gridPoint
                |> Maybe.withDefault emptyField

        ( newFieldState, shipIdTreffer ) =
            case ( currentFieldState.shipId, currentFieldState.shoot ) of
                ( _, True ) ->
                    ( currentFieldState, Nothing )

                ( Nothing, False ) ->
                    ( { currentFieldState | shoot = True }, Nothing )

                ( Just shipId, False ) ->
                    ( { currentFieldState | shoot = True }, Just shipId )

        gameFieldNew =
            Dict.insert gridPoint newFieldState model.gameField

        length shipId =
            model.ships
                |> Dict.get shipId
                |> Maybe.map .length
                |> Maybe.withDefault 0

        shipDestroyed =
            case shipIdTreffer of
                Nothing ->
                    False

                Just shipId_ ->
                    gameFieldNew
                        |> Dict.values
                        |> List.filter (\{ shipId, shoot } -> shipId == Just shipId_ && shoot == True)
                        |> List.length
                        |> (==) (length shipId_)

        shipsNew =
            case ( shipDestroyed, shipIdTreffer ) of
                ( False, _ ) ->
                    model.ships

                ( True, Nothing ) ->
                    model.ships

                ( True, Just shipId ) ->
                    Dict.update shipId
                        (\value ->
                            case value of
                                Nothing ->
                                    Nothing

                                Just s ->
                                    Just { s | destroyed = True }
                        )
                        model.ships

        alleShipsDestroyed =
            shipsNew
                |> Dict.values
                |> List.all .destroyed
    in
    ( if model.state == Finished then
        model
      else
        { model
            | gameField = gameFieldNew
            , ships = shipsNew
            , state =
                if alleShipsDestroyed then
                    Finished
                else
                    model.state
        }
    , Cmd.none
    )


buildGameField : List ( Position, Bool ) -> Model -> ( Model, Cmd Msg )
buildGameField rs model =
    let
        random : List ( Position, Direction )
        random =
            rs
                |> List.map (\( n, b ) -> ( n, directionFromBool b ))

        place :
            Int
            -> ShipLength
            -> List ( Position, Direction )
            -> ( GameField, Ships )
            -> ( List ( Position, Direction ), ( GameField, Ships ), Maybe Int )
        place shipId shipLength randomList ( gameField, ships ) =
            case randomList of
                [] ->
                    -- nicht alle ships konnten plaziert werden
                    ( randomList, ( gameField, ships ), Just shipLength )

                ( pos, richt ) :: zs ->
                    if fitsShipIntoTheGameField richt shipLength gameFieldSize pos then
                        case tryToPlaceShip pos richt shipLength shipId ( gameField, ships ) of
                            Just ( gameField_, ships_ ) ->
                                ( zs, ( gameField_, ships_ ), Nothing )

                            Nothing ->
                                place shipId shipLength zs ( gameField, ships )
                    else
                        place shipId shipLength zs ( gameField, ships )

        shipsIds : List ( ShipId, ShipLength )
        shipsIds =
            ships |> List.concatMap (\( n, length ) -> List.repeat n length) |> List.indexedMap (\i length -> ( i, length ))

        ( zs, ( gameField, ships2 ), rest ) =
            shipsIds
                |> -- foldr : ((ShipId,Length) -> (d1,d2) -> (d1,d2)) -> (d1,d2) -> List (ShipId,Length) -> (d1,d2)
                   List.foldr
                    (\( shipId, shipLength ) ( randomList1, ( dict1, dict2 ), ok ) ->
                        place shipId shipLength randomList1 ( dict1, dict2 )
                    )
                    ( random, ( Dict.empty, Dict.empty ), Nothing )
    in
    -- wenn nicht alle ships plaziert wurden => new versuch
    if rest == Nothing then
        ( { model
            | gameField = gameField
            , ships = ships2
          }
        , Cmd.none
        )
    else
        Debug.log "no solution found" ( model, initialRandom )


possibleShipPositions : Position -> ShipLength -> Direction -> List Position
possibleShipPositions ( x, y ) shipLength richtung =
    case richtung of
        WO ->
            List.map2 (,) (List.range x (x + shipLength - 1)) (List.repeat shipLength y)

        NS ->
            List.map2 (,) (List.repeat shipLength x) (List.range y (y + shipLength - 1))


isEnoughSpaceForShipField : GameField -> Position -> Bool
isEnoughSpaceForShipField gameField ( x, y ) =
    let
        around =
            []
                ++ [ ( x - 1, y - 1 ), ( x + 0, y - 1 ), ( x + 1, y - 1 ) ]
                ++ [ ( x - 1, y + 0 ), ( x + 0, y + 0 ), ( x + 1, y + 0 ) ]
                ++ [ ( x - 1, y + 1 ), ( x + 0, y + 1 ), ( x + 1, y + 1 ) ]
    in
    around
        |> List.map (\p -> Dict.get p gameField)
        |> List.all (\m -> m == Nothing)


isEnoughSpaceForShip : List Position -> GameField -> Bool
isEnoughSpaceForShip pos gameField =
    pos
        |> List.map (isEnoughSpaceForShipField gameField)
        |> List.all (\m -> m == True)


tryToPlaceShip : Position -> Direction -> ShipLength -> ShipId -> ( GameField, Ships ) -> Maybe ( GameField, Ships )
tryToPlaceShip pos dir shipLength shipId ( gameField, ships ) =
    let
        positions : List Position
        positions =
            possibleShipPositions pos shipLength dir
    in
    if isEnoughSpaceForShip positions gameField then
        positions
            |> List.foldr
                (\p ( gameField_, ships_ ) ->
                    ( Dict.insert p { shipId = Just shipId, shoot = False } gameField_
                    , Dict.insert shipId { shipId = shipId, length = shipLength, destroyed = False } ships_
                    )
                )
                ( gameField, ships )
            |> Just
    else
        Nothing


allFieldCoordinates : FieldSize -> List Position
allFieldCoordinates n =
    List.range 0 n |> List.concatMap (\x -> List.range 0 n |> List.map (\y -> ( x, y )))


placeShip : Direction -> ShipLength -> FieldSize -> List Position
placeShip richtung shipLength fieldSize =
    allFieldCoordinates fieldSize
        |> List.filter (fitsShipIntoTheGameField richtung shipLength fieldSize)


fitsShipIntoTheGameField : Direction -> ShipLength -> FieldSize -> Position -> Bool
fitsShipIntoTheGameField richtung shipLength fieldSize ( x, y ) =
    let
        xOrY =
            case richtung of
                WO ->
                    x

                NS ->
                    y
    in
    xOrY + shipLength - 1 <= fieldSize


viewState : Model -> Html Msg
viewState model =
    Select.custom
        [ Select.id "myselect"
        , Select.onChange StateChanged
        ]
        (List.map
            (\z ->
                Select.item
                    [ Html.Attributes.value <| toString z
                    , Html.Attributes.selected (z == model.state)
                    ]
                    [ Html.text <| toString z ]
            )
            allGameStates
        )


type alias ViewConfig =
    { boardSize : Int
    , fieldCount : Int
    , fieldSize : Int
    , fieldOffset : Int
    , windowWidth : Int
    , windowHeight : Int
    , halbeFontSize : Int
    , border : Int
    , fontSize : Float
    , space : Int
    , startX : Int
    , startY : Int
    }


viewConfig : Int -> Int -> ViewConfig
viewConfig screenSizeWidth screenSizeHeight =
    let
        maxWidthAndHeight =
            1400

        currentWidthAndHeight =
            toFloat <|
                Basics.min 1400 <|
                    Basics.min screenSizeWidth screenSizeHeight

        fontScale =
            Scale.linear ( 400, maxWidthAndHeight ) ( 7, 22 )

        fontSize : Float
        fontSize =
            Scale.convert fontScale currentWidthAndHeight

        halbeFontSize =
            Basics.round <|
                fontSize
                    / 2.0

        space =
            fieldSize // 5

        fieldCount =
            gameFieldSize + 1

        windowWidth =
            screenSizeWidth - 200

        windowHeight =
            screenSizeHeight - 200

        ( startX, startY ) =
            ( 50, 50 )

        boardSize1 =
            Basics.min (windowWidth - startX) (windowHeight - startY - 100)

        border =
            1

        fieldSize =
            (boardSize1 - 2 * border - (fieldCount - 1) * border) // fieldCount

        boardSize =
            fieldSize * fieldCount + border * (fieldCount + 1)

        fieldOffset =
            fieldSize + border
    in
    { startX = startX
    , startY = startY
    , border = border
    , fieldOffset = fieldOffset
    , fieldSize = fieldSize
    , fontSize = fontSize
    , halbeFontSize = halbeFontSize
    , fieldCount = fieldCount
    , boardSize = boardSize
    , windowWidth = windowWidth
    , windowHeight = windowHeight
    , space = space
    }


fieldColorAndText : Model -> ( Int, Int ) -> ( String, String )
fieldColorAndText model ( ix, iy ) =
    let
        fieldState =
            model.gameField
                |> Dict.get ( ix, iy )
                |> Maybe.withDefault emptyField
    in
    case ( model.state, fieldState.shipId |> Maybe.andThen (\id -> Dict.get id model.ships), fieldState.shoot ) of
        ( ShowShips, Nothing, True ) ->
            ( ColorHelper.white, "x" )

        ( ShowShips, Nothing, False ) ->
            ( ColorHelper.white, "" )

        ( ShowShips, Just ship, False ) ->
            ( ColorHelper.colorNr ship.length, toString ship.shipId )

        ( ShowShips, Just ship, True ) ->
            ( ColorHelper.colorNr ship.length, "[" ++ toString ship.shipId ++ "]" )

        ( Normal, Just { length, destroyed }, True ) ->
            if destroyed then
                ( ColorHelper.colorNr length, "" )
            else
                ( ColorHelper.grey, "" )

        ( Normal, Nothing, True ) ->
            ( ColorHelper.white, "o" )

        ( Normal, _, False ) ->
            ( ColorHelper.white, "" )

        ( Finished, Just { length, destroyed }, True ) ->
            if destroyed then
                ( ColorHelper.colorNr length, "" )
            else
                ( ColorHelper.grey, "" )

        ( Finished, Nothing, True ) ->
            ( ColorHelper.white, "o" )

        ( Finished, _, False ) ->
            ( ColorHelper.white, "" )

        ( PlaceShips, _, _ ) ->
            ( ColorHelper.white, "" )


view : Model -> Html Msg
view model =
    let
        c =
            viewConfig model.screenSize.width model.screenSize.height

        s =
            toString

        -- --- FELDER
        field ( ix, iy ) =
            let
                ( fieldColor, fieldText ) =
                    fieldColorAndText model ( ix, iy )
            in
            [ rect
                [ x (s <| c.startX + c.border + ix * c.fieldOffset)
                , y (s <| c.startY + c.border + iy * c.fieldOffset)
                , width (s c.fieldSize)
                , height (s c.fieldSize)
                , fill fieldColor
                , Svg.Events.on "click" (Json.succeed <| FieldClicked ( ix, iy ))
                ]
                []
            , text_
                [ fontSize <| s c.fontSize
                , x (s <| c.startX + c.border + (c.fieldSize // 2) + ix * c.fieldOffset)
                , y
                    (s <| c.startY + c.halbeFontSize + c.border + (c.fieldSize // 2) + iy * c.fieldOffset)
                ]
                [ text <| fieldText ]
            ]

        fields =
            allFieldCoordinates (c.fieldCount - 1) |> List.concatMap field

        -- --- HINTERGRUND
        background =
            [ rect
                [ x <| s c.startX
                , y <| s c.startY
                , width (s c.boardSize)
                , height (s c.boardSize)
                , fill "lightgrey"
                ]
                []
            ]

        -- --- ACHSEN
        toChar n =
            n + Char.toCode 'A' |> Char.fromCode |> String.fromChar

        xAxisMarker ix =
            [ text_
                [ fontSize <| s c.fontSize
                , x (s <| c.startX + c.border + (c.fieldSize // 2) + ix * c.fieldOffset)
                , y (s <| c.startY - 1 * c.space)
                ]
                [ text <| toString ix ]
            ]

        xAxis =
            List.range 0 (c.fieldCount - 1) |> List.concatMap xAxisMarker

        yAxisMarker iy =
            [ text_
                [ fontSize <| s c.fontSize
                , x (s <| toFloat c.startX - 4 * toFloat c.space)
                , y (s <| c.startY + c.halbeFontSize + 2 * c.border + (c.fieldSize // 2) + iy * c.fieldOffset)
                ]
                [ text <| toChar iy ]
            ]

        yAxis =
            List.range 0 (c.fieldCount - 1) |> List.concatMap yAxisMarker

        shipListe : Model -> List ( ShipLength, Int, Int )
        shipListe model =
            model.ships
                |> Dict.values
                |> DictExtra.groupBy .length
                |> Dict.map (\l ss -> ( l, List.length ss, ss |> List.filter .destroyed |> List.length ))
                |> Dict.values
    in
    Html.div []
        [ CDN.stylesheet
        , Grid.containerFluid
            []
            [ Grid.row []
                [ Grid.col [ Col.sm8, Col.lg8 ] [ Html.h3 [] [ text "Battleships" ] ]
                , Grid.col [] [ viewState model ]
                ]
            , Grid.row []
                [ Grid.col [ Col.sm9, Col.lg9 ]
                    [ svg
                        [ width <| s c.windowWidth
                        , height <| s c.windowHeight
                        ]
                        ([]
                            ++ background
                            ++ xAxis
                            ++ yAxis
                            ++ fields
                        )
                    ]
                , Grid.col [ Col.sm1, Col.lg1 ]
                    (if model.state /= Finished then
                        [ shipListe model |> viewShipsTable ]
                     else
                        [ Button.button [ Button.primary, Button.onClick Restarted ] [ text "Primary" ] ]
                    )
                ]
            ]
        ]


viewShipsTable : List ( ShipLength, Int, Int ) -> Html Msg
viewShipsTable ships =
    Table.table
        { options = [ Table.striped, Table.hover, Table.small ]
        , thead =
            Table.simpleThead
                [ Table.th [] [ text "Length" ]
                , Table.th [] [ text "Count" ]
                , Table.th [] [ text "Destroyed" ]
                ]
        , tbody =
            Table.tbody []
                (ships
                    |> List.map
                        (\( length, count, destroyed ) ->
                            Table.tr []
                                [ Table.td [] [ text <| toString length ]
                                , Table.td [] [ text <| toString count ]
                                , Table.td [] [ text <| toString destroyed ]
                                ]
                        )
                )
        }



-- -----------------------------------------


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes (\size -> ScreenSizeChanged size)


initialSize : Cmd Msg
initialSize =
    Window.size
        |> Task.perform ScreenSizeChanged


initialRandom : Cmd Msg
initialRandom =
    Random.generate
        RandomListGenerated
        (Random.list 500
            (Random.pair
                (Random.pair
                    (Random.int 0 gameFieldSize)
                    (Random.int 0 gameFieldSize)
                )
                Random.bool
            )
        )


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.batch [ initialSize, initialRandom ] )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
